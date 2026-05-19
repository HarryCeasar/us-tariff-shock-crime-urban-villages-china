import re

with open('report.tex', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace "\section{犯罪案由异质性}" with "\section{分案由的进一步讨论}"
content = content.replace(r'\section{犯罪案由异质性}', r'\section{分案由的进一步讨论}')

# Replace text inside the section
content = content.replace(r'具有较为清晰的案由异质性', r'具有较为清晰的案由结构差异')

# Replace table 4 title
content = content.replace(r'\caption{案由大类异质性}', r'\caption{案由大类的进一步讨论}')

# Replace table 5 title
content = content.replace(r'\caption{案由异质性}', r'\caption{16类细分案由的进一步讨论}')

with open('report.tex', 'w', encoding='utf-8') as f:
    f.write(content)

print("Replacement done.")
