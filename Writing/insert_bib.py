with open('report.tex', 'r', encoding='utf-8') as f:
    text = f.read()

bib_text = r"""
\clearpage
\printbibliography[keyword=zh,heading=bibintoc,title={中文参考文献}]
\printbibliography[keyword=en,heading=bibintoc,title={外文参考文献}]

\clearpage
\appendix
\chapter*{附录}
"""

text = text.replace(r"""\clearpage
\appendix
\chapter*{附录}""", bib_text)

with open('report.tex', 'w', encoding='utf-8') as f:
    f.write(text)

print("Bibliography inserted before Appendix.")
