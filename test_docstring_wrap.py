# Copyright (C) 2012
# See LICENSE.txt for details.

"""
Tests for docstring_wrap.py.
"""

try:
    # For Python 2.6
    import unittest2 as unittest
except ImportError:
    import unittest

from docstring_wrap import main, wrapPythonDocstring


class FunctionalTests(unittest.TestCase):
    """
    Functional tests for the wrapper.
    """
    def test_self(self):
        """
        This module's, class's, & method's docstrings are fine and therefore
        not mangled.  They're all interesting because they have different
        indentations.
        """
        for ds in (__doc__, self.__class__.__doc__, self.test_self.__doc__):
            self.assertEqual(
                ds,
                main(["test"], ds)
            )

    def test_epytext_nop(self):
        """
        wrapPythonDocstring has an impressive multi-paragraph docstring full of
        epytext and doesn't get mangled.
        """
        self.assertEqual(
            wrapPythonDocstring.__doc__,
            main(["test"], wrapPythonDocstring.__doc__)
        )

    @unittest.expectedFailure
    def test_sphinx_nop(self):
        """
        Long and Sphinx- (and docstring!)-rich docstrings don't get mangled.
        """
        ds = \
    """
    Phasellus purus.

    :param int arg: Cras placerat accumsan nulla.

    >>> print("hello")
    hello

    Aliquam erat volutpat.  Nunc eleifend leo vitae magna.  In id erat non orci
    commodo lobortis.  Proin neque massa, cursus ut, gravida ut, lobortis eget,
    lacus.  Sed diam.  Praesent fermentum tempor tellus.  Nullam tempus.
    """
        self.assertEqual(
            ds,
            main(["test"], ds)
        )

    def test_single_line_too_wide(self):
        """
        Overly long single line docstrings get refilled correctly.
        """
        ds = """
        This is totally too long and must be refilled.  Fortunately we have an awesome plugin for that!
        """  # noqa
        self.assertEqual(
            """
        This is totally too long and must be refilled.  Fortunately we have an
        awesome plugin for that!
        """,
            main(["test"], ds)
        )

    def test_single_space_used_when_specified(self):
        """
        When called with --single-space, only a single space is inserted at
        the end of sentences.
        """
        ds = """
        Sentence number one. Sentence number two.
        """
        self.assertEqual(
            ds,
            main(["test", "--single-space"], ds))


if __name__ == '__main__':
    unittest.main()
