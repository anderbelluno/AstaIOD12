import sys
import re

def trace_balance(filepath):
    with open(filepath, 'r') as f:
        lines = f.readlines()

    stack = []
    
    # Simple regexes
    starter_re = re.compile(r'\b(begin|case|record|asm)\b', re.IGNORECASE)
    end_re = re.compile(r'\bend\b', re.IGNORECASE)
    class_re = re.compile(r'=\s*class\b', re.IGNORECASE)
    string_re = re.compile(r"'.*?'")
    
    impl_start = -1
    for i, line in enumerate(lines):
        if 'implementation' in line.lower():
            impl_start = i
            break
            
    if impl_start == -1: return

    for i in range(impl_start, len(lines)):
        line = lines[i]
        # Remove comments
        clean_line = re.sub(r'//.*', '', line)
        # Note: multi-line comments not handled perfectly here but usually they don't contain keywords
        
        # Remove strings
        clean_line = string_re.sub("''", clean_line)
        
        # Check for starters
        for match in starter_re.finditer(clean_line):
            stack.append((match.group(1).lower(), i + 1))
            
        for match in class_re.finditer(clean_line):
            stack.append(('class', i + 1))
            
        # Check for ends
        for match in end_re.finditer(clean_line):
            if stack:
                starter, start_line = stack.pop()
                # print(f"Match: {starter} at {start_line} closed by end at {i+1}")
            else:
                print(f"EXTRA END found at line {i+1}")

    if stack:
        for starter, line in stack:
            print(f"UNCLOSED {starter} at line {line}")

if __name__ == "__main__":
    trace_balance(sys.argv[1])
