import sys

def check_brackets_balance(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Simple stack for brackets
    stack = []
    brackets = {'(': ')', '[': ']', '{': '}'}
    
    in_string = False
    in_line_comment = False
    in_comment = False # (* *)
    
    for i, char in enumerate(content):
        # Handle strings
        if char == "'" and not in_line_comment and not in_comment:
            if not in_string or (i > 0 and content[i-1] != "'"): # simplified string handling
                in_string = not in_string
            continue
            
        if in_string: continue
        
        # Handle comments
        if char == '/' and i < len(content)-1 and content[i+1] == '/' and not in_comment:
            in_line_comment = True
        if char == '\n':
            in_line_comment = False
            
        if char == '(' and i < len(content)-1 and content[i+1] == '*' and not in_line_comment:
            in_comment = True
        if char == '*' and i < len(content)-1 and content[i+1] == ')' and in_comment:
            in_comment = False
            # skip the next char
            continue
            
        if in_line_comment or in_comment: continue
        
        # Special case for Delphi directives {$IF} etc. 
        # They use { } but we should count them as brackets if we want to be safe,
        # but usually they are balanced separately. 
        # However, Pascal comments { } are also brackets in this sense.
        
        if char in brackets.keys():
            stack.append((char, i))
        elif char in brackets.values():
            if not stack:
                print(f"Extra closing bracket {char} at index {i}")
            else:
                top, pos = stack.pop()
                if brackets[top] != char:
                    print(f"Mismatch: {top} at {pos} closed by {char} at {i}")

    for char, pos in stack:
        # Find line number for pos
        line_no = content.count('\n', 0, pos) + 1
        print(f"Unclosed {char} at line {line_no}")

if __name__ == "__main__":
    check_brackets_balance(sys.argv[1])
