import sys
import re

def audit_function_bodies(filepath):
    with open(filepath, 'r') as f:
        lines = f.readlines()

    impl_start = -1
    for i, line in enumerate(lines):
        if 'implementation' in line.lower():
            impl_start = i
            break
            
    if impl_start == -1: return

    # Simple regex for header
    header_re = re.compile(r'^\s*(function|procedure)\b', re.IGNORECASE)
    
    for i in range(impl_start, len(lines)):
        line = lines[i]
        if header_re.match(line):
            # Check if it has a body
            has_body = False
            for j in range(i, min(i + 20, len(lines))):
                if 'begin' in lines[j].lower() or 'asm' in lines[j].lower():
                    has_body = True
                    break
                if 'forward;' in lines[j].lower():
                    has_body = True
                    break
            if not has_body:
                print(f"HEADER WITHOUT BODY at line {i+1}: {line.strip()}")

if __name__ == "__main__":
    audit_function_bodies(sys.argv[1])
