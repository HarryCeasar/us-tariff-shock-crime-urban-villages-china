with open('report.tex', 'r', encoding='utf-8') as f:
    text = f.read()

en_abs_short = r"""\begin{enabstract}
The social spillover of macroeconomic shocks is a central focus in development economics. As vital hubs accommodating migrant labor, urban villages in the Pearl River Delta face substantial risks during such economic fluctuations. Exploiting the 2018 U.S. tariff escalation against China as a quasi-natural experiment, this dissertation constructs a 100-meter grid half-year panel dataset comprising both formal blocks and urban villages using criminal court judgments, natural language processing, and geocoding. Using a Poisson Pseudo-Maximum Likelihood (PPML) difference-in-differences framework, this study identifies the localized public-safety pressures and their micro-spatial distribution following the exogenous tariff shock.

Empirical results indicate that the tariff shock significantly elevates crime intensity within urban villages. A one-standard-deviation increase in county-level tariff exposure drives crime volume in urban villages up by approximately 20.6%, whereas formal neighborhoods show no comparable response. This micro-level crime surge exhibits an uneven spatial distribution, primarily clustering in peripheral urban villages located along the urban fringes, while inner-city core urban villages show relatively weak responses. Specifically, the shock notably increases economically driven property crimes, friction-induced impulsive offenses, and certain underground crimes. Supplementary analyses suggest that community-level social connectivity indicators (e.g., catering density) provide partial buffering capabilities, whereas proximity to high-traffic public facilities does not exhibit similar mitigating effects.

This study enriches the literature on the social impacts of international trade frictions from a micro-economic geography perspective. By narrowing the analytical scale to 100-meter residential grids, the research demonstrates how localized governance costs spawned by economic contraction are primarily absorbed by marginalized micro-units with weaker foundational conditions. These findings offer practical policy implications: during macroeconomic fluctuations, municipal public safety and community governance resources should phase out blanket interventions and pivot toward targeted support for highly vulnerable nodes, particularly peripheral urban villages. Systematically routing social safety nets into these fragile networks can more effectively defuse regional social tensions induced by external shocks.
\end{enabstract}"""

def replace_between(text, start_tag, end_tag, replacement):
    start = text.find(start_tag)
    end = text.find(end_tag) + len(end_tag)
    if start != -1 and end != -1:
        return text[:start] + replacement + text[end:]
    return text

text_new = replace_between(text, '\\begin{enabstract}', '\\end{enabstract}', en_abs_short)

with open('report.tex', 'w', encoding='utf-8') as f:
    f.write(text_new)

print("English abstract updated.")
