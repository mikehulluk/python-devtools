
from yapsy.IPlugin import IPlugin
import re


class LyxRefFormatter(IPlugin):

    def action_text(self, text):
        # Convert all citations to the correct form:
        text = re.sub(r"""^LatexCommand cite$""", r"""LatexCommand citep""", text, flags=re.MULTILINE )
        text = re.sub(r"""^LatexCommand citet$""", r"""LatexCommand citep""",text, flags=re.MULTILINE )

        # Ensure that there is a space between [a-zA-Z] and citation.
        spacing = re.compile( r"""([a-zA-Z])(\n\\begin_inset CommandInset citation)""", re.MULTILINE|re.DOTALL)
        text = spacing.sub(r"""\1 \2""", text)
        return text
