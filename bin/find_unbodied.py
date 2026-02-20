import sys
import re

def find_unbodied_functions(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Remove all comments for analysis
    # // comments
    content = re.sub(r'//.*', '', content)
    # { } comments (excluding directives)
    content = re.sub(r'{[^\$].*?}', '', content, flags=re.DOTALL)
    # (* *) comments
    content = re.sub(r'\(\*.*?\*\)', '', content, flags=re.DOTALL)
    
    # Also ignore what's inside strings
    content = re.sub(r"'.*?'", "''", content)

    lines = content.split('\n')
    impl_start = -1
    for i, line in enumerate(lines):
        if 'implementation' in line.lower():
            impl_start = i
            break
            
    if impl_start == -1:
        print("Implementation section not found.")
        return

    # Process from implementation start
    # We want to find function/procedure headers that aren't 'forward;'
    # and don't have a body.
    
    for i in range(impl_start, len(lines)):
        line = lines[i].strip()
        if not line: continue
        
        # Look for procedure/function start
        if re.match(r'^\s*(procedure|function)\b', line, re.IGNORECASE):
            # Signature might span multiple lines
            full_sig = line
            curr_i = i
            while ';' not in full_sig and curr_i < len(lines) - 1:
                curr_i += 1
                full_sig += ' ' + lines[curr_i].strip()
            
            if 'forward;' in full_sig.lower():
                continue
                
            # Now we have the full signature. Check what follows.
            found_body = False
            for j in range(curr_i + 1, min(curr_i + 50, len(lines))):
                next_line = lines[j].strip().lower()
                if not next_line: continue
                
                # Check for body starters or other section starters
                if next_line.startswith('begin') or next_line.startswith('asm') or \
                   next_line.startswith('var') or next_line.startswith('const') or \
                   next_line.startswith('type') or next_line.startswith('label'):
                    found_body = True
                    break
                
                # If we hit another function/procedure/section without finding a body
                if next_line.startswith('procedure') or next_line.startswith('function') or \
                   next_line.startswith('initialization') or next_line.startswith('finalization') or \
                   next_line.startswith('end.'):
                    break
            
            if not found_body:
                print(f"UNCLEAN Signature at line {i+1}: {full_sig}")

if __name__ == "__main__":
    find_unbodied_functions(sys.argv[1])
