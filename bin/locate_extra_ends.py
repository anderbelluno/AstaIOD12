import sys
import re

def locate_extra_ends(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Simple logic
    stack = []
    # starters that end with 'end'
    # class, record, begin, case, try, asm
    starter_re = re.compile(r'\b(begin|case|record|asm|try)\b', re.IGNORECASE)
    class_re = re.compile(r'=\s*class\b', re.IGNORECASE)
    end_re = re.compile(r'\bend\b', re.IGNORECASE)
    
    # Remove strings and comments
    content = re.sub(r'//.*', '', content)
    content = re.sub(r'{[^\$].*?}', '', content, flags=re.DOTALL)
    content = re.sub(r'\(\*.*?\*\)', '', content, flags=re.DOTALL)
    content = re.sub(r"'.*?'", "''", content)
    
    lines = content.split('\n')
    impl_start = -1
    for i, line in enumerate(lines):
        if 'implementation' in line.lower():
            impl_start = i
            break
            
    if impl_start == -1: return

    for i in range(impl_start, len(lines)):
        line = lines[i]
        
        # Starters
        for match in starter_re.finditer(line):
            stack.append((match.group(1).lower(), i + 1))
        for match in class_re.finditer(line):
            stack.append(('class', i + 1))
            
        # Ends
        for match in end_re.finditer(line):
            if stack:
                starter, start_line = stack.pop()
            else:
                if 'end.' not in line.lower():
                    print(f"EXTRA END at line {i+1}: {line.strip()}")

if __name__ == "__main__":
    locate_extra_ends(sys.argv[1])
