# Copyright (C) 2012
# See LICENSE.txt for details.

"""
General Python docstring wrapper
================================

Utility for wrapping docstrings in Python; specifically, docstrings in
U{Epytext <http://epydoc.sourceforge.net/manual-epytext.html>} or Sphinx
ReStructureText format.

The wrapping herein generally adheres to all the conventions set forth by the
Twisted project U{http://twistedmatrix.com/}, but should be generally accurate
for most Python projects.
"""

from __future__ import unicode_literals

import argparse
import sys
import re

from io import StringIO
from uuid import uuid4


__all__ = [
    "wrapPythonDocstring"
]


if sys.version_info[0] <= 2:
    makeID = lambda: unicode(uuid4())
    fromStdin = lambda s: s.decode("utf-8")
    toStdout = lambda s: s.encode("utf-8")
    PY2 = True
else:
    makeID = lambda: str(uuid4())
    fromStdin = lambda s: s
    toStdout = lambda s: s
    PY2 = False



def isUnderline(expr):
    return bool(re.match("[=]+$", expr) or re.match("[-]+$", expr))



def startslist(x):
    return (x == '-' or (x.endswith(".") and (x[:-1].isdigit()
                                              or x[:-1] == '#')))



def isAcronym(word):
    """
    Is the given word an acronym (separated by periods, so it doesn't end a
    sentence)?  cf. lots of interesting acronyms, e.g. this is one.  solve for
    x.  a.b.c. is also one.  You might also want to give an example
    parenthetically (e.g. this one).
    """
    word = word.strip("(")
    return ((len(word) > 2 and word[1::2] == '.' * int(len(word) / 2)) or
            word in ["cf.", "viz."])



def isSentenceEnd(prevWord):
    """
    Is the given word the end of a sentence?
    """
    if not prevWord:
        return False
    # Exclamation points and question marks generally end sentences.
    if prevWord[-1] in "?!":
        return True
    # Now, if it's not a period, it's probably not the end of a sentence.
    if prevWord[-1] != ".":
        return False
    # Don't insert sentence-end breaks into sphinx markup things.
    if prevWord == '..':
        return False
    if isAcronym(prevWord):
        return False
    return True



def beginsField(line):
    """
    Does the given (stripped) line begin an epytext or ReST field?
    """
    if line.startswith("@"):
        return True
    sphinxwords = """
    param params return type rtype summary var ivar cvar raises raise except
    exception
    """.split()
    for word in sphinxwords:
        if line.startswith(":" + word):
            return True
    return False



class RegularParagraph(object):
    otherIndent = ""

    def __init__(self, pointTracker, fixedIndent="", hangIndent="",
                 followIndent="", originalIndent=0):
        self.words = []
        self.fixedIndent = fixedIndent
        self.hangIndent = hangIndent
        self.followIndent = followIndent
        self.more = None
        self.prev = None
        self.pointTracker = pointTracker
        # originalIndent is the width of the indentation of the line this
        # paragraph originally came from in the input text.
        self.originalIndent = originalIndent
        self._unwrappedLines = 0
        self._headingType = None
        self._headingPoints = []


    def matchesTag(self, other):
        return False


    def __nonzero__(self):
        return bool(self.words)


    def all(self):
        while self is not None:
            #print self.__class__.__name__
            if self:
                yield self
            self = self.more


    def setIsHeading(self, headingType):
        self._headingType = headingType


    def isHeading(self):
        return bool(self._headingType)

    def connect(self, more):
        self.more = more
        more.prev = self
        return more

    def islist(self):
        return self.words and startslist(self.words[0])

    def previousListPeer(self):
        """
        Find a previous paragraph that is also a list element, of the same
        indentation level if one exists.
        """
        previous = self.prev
        matched = None
        while previous:
            if not previous.words:
                previous = previous.prev
                continue
            if not previous.islist():
                break
            if previous.originalIndent <= self.originalIndent:
                return previous
            if previous.originalIndent > self.originalIndent:
                matched = previous
            previous = previous.prev
        if matched:
            return matched

    def add(self, line):
        clean = self.pointTracker.peek(line)
        stripped = clean.strip()
        thisLineIndent = len(clean) - len(clean.lstrip())

        if stripped:
            self._unwrappedLines += 1
            active = self
            firstword = list(self.pointTracker.filterWords(line.split()))[0]
            if beginsField(stripped):
                fp = FieldParagraph(pointTracker=self.pointTracker, originalIndent=thisLineIndent)
                fp.words.extend(line.split())
                active = active.connect(fp)
            elif isUnderline(stripped) and self._unwrappedLines == 2:
                # This paragraph is actually a section heading.
                active.setIsHeading(stripped[0])
                self._headingPoints = self.pointTracker.extractPoints(line)
                # FIXME: should respect leading indentation.
                active = active.connect(self.genRegular(originalIndent=thisLineIndent))
            elif startslist(firstword):
                # Aesthetically I prefer a 2-space indent here, but the
                # convention in the codebase seems to be 4 spaces.
                LIST_INDENT = 4
                # FIXME: this also needs to respect leading indentation so it
                # can properly represent nested lists.
                hangIndent = self.pointTracker.lengthOf(firstword) + 1
                fi = self.fixedIndent
                if not (self.words and startslist(self.words[0])):
                    fi += (" " * LIST_INDENT)
                fp = RegularParagraph(
                    pointTracker=self.pointTracker,
                    fixedIndent=fi,
                    hangIndent=" " * hangIndent,
                    followIndent=self.followIndent,
                    originalIndent=thisLineIndent,
                )
                fp.words.extend(line.split())
                fp.prev = self
                peer = fp.previousListPeer()
                if peer:
                    if peer.originalIndent >= fp.originalIndent:
                        fp.fixedIndent = peer.fixedIndent
                    else:
                        fp.fixedIndent = peer.fixedIndent + (" " * LIST_INDENT)
                active = active.connect(fp)
            else:
                self.words.extend(line.split())
            if stripped.endswith("::") or (".. code-block:: " in stripped):
                active = active.connect(PreFormattedParagraph(
                    active,
                    indentBegins=thisLineIndent
                ))
            return active
        else:
            rawstrip = line.strip()
            if rawstrip:
                self.words.append(rawstrip)
            if len(list(self.pointTracker.filterWords(self.words))):
                return self.connect(self.genRegular(originalIndent=thisLineIndent))
        return self


    def wrap(self, output, indentation, width, initialBlank, singleSpace):
        maxWidthThisLine = width
        if not self.words:
            return
        if initialBlank:
            thisLine = self.firstIndent(indentation)
        else:
            thisLine = ''
            maxWidthThisLine -= (3 + len(indentation))
        first = True
        prevWord = ''
        for num, word in enumerate(self.words):
            if not self.pointTracker.isWord(word):
                thisLine += word
                continue
            normalPrevWord = self.pointTracker.peek(prevWord)
            if num == 1 and startslist(normalPrevWord):
                spaces = 1
            elif isSentenceEnd(normalPrevWord) and singleSpace:
                spaces = 2
            else:
                spaces = 1
            prevWord = word
            thisLineWidthWithThisWord = (self.pointTracker.lengthOf(thisLine) +
                                         self.pointTracker.lengthOf(word) +
                                         spaces)
            if thisLineWidthWithThisWord <= maxWidthThisLine or first:
                if first:
                    first = not first
                else:
                    thisLine += (" " * spaces)
                thisLine += word
            else:
                output.write(self.pointTracker.scan(thisLine, output.tell()))
                output.write("\n")
                maxWidthThisLine = width
                thisLine = self.restIndent(indentation) + word
        output.write(self.pointTracker.scan(thisLine, output.tell()))
        output.write("\n")
        if self.isHeading():
            indentText = self.firstIndent(indentation)
            lineSize = self.pointTracker.lengthOf(thisLine) - len(indentText)
            output.write(self.pointTracker.scan(
                indentText + ''.join(self._headingPoints) +
                (self._headingType * lineSize), output.tell()
            ))
            output.write("\n")


    def firstIndent(self, indentation):
        return indentation + self.fixedIndent


    def restIndent(self, indentation):
        return (indentation + self.fixedIndent + self.hangIndent +
                self.otherIndent)


    def genRegular(self, originalIndent=0):
        return RegularParagraph(pointTracker=self.pointTracker,
                                fixedIndent=self.nextIndent(),
                                followIndent=self.nextIndent(),
                                originalIndent=originalIndent)


    def nextIndent(self):
        return self.followIndent



class FieldParagraph(RegularParagraph):

    @property
    def otherIndent(self):
        """
        Compute the other indent appropriate to the length of a sphinx field,
        if we're wrapping a sphinx field.
        """
        if self.words[0].startswith(':'):
            accumulatedLength = 0
            for word in self.words:
                word = self.pointTracker.peek(word)
                # Add the length of the word
                accumulatedLength += len(word)
                # Add the following space
                accumulatedLength += 1
                # If it gets too long then give up and go with the default.
                if accumulatedLength > 10:
                    break
                if word.endswith(":"):
                    return accumulatedLength * " "
        return "    "


    def nextIndent(self):
        return "    "


    def matchesTag(self, other):
        if isinstance(other, FieldParagraph):
            myWords = list(self.pointTracker.filterWords(self.words))
            theirWords = list(self.pointTracker.filterWords(other.words))
            if ( set([myWords[0], theirWords[0]]) ==
                 set(["@return:", "@rtype:"]) ):
                 # matching @return and @rtype fields.
                return True
            elif myWords[0][0] == theirWords[0][0] == ':':
                # hack for sphinx: prevailing style seems to be 'group @params
                # together'
                if myWords[0] == theirWords[0]:
                    return True
                elif ( set([myWords[0], theirWords[0]]) ==
                       set([":return:", ":rtype:"]) ):
                    return True
                elif ( set([myWords[0], theirWords[0]]) ==
                       set([":param", ":type"]) and
                       len(myWords) > 1 and len(theirWords) > 1 and
                       myWords[1] == theirWords[1]):
                    # same as "matching @param and @type" below, but stricter;
                    # FIXME: these should be merged.
                    return True
                else:
                    return False
            elif len(myWords) > 1 and len(theirWords) > 1:
                # matching @param and @type fields.
                return myWords[1] == theirWords[1]
            return False
        else:
            return False



class PreFormattedParagraph(object):

    def __init__(self, before, indentBegins):
        self.lines = []
        self.before = before

        pointTracker = before.pointTracker

        fixedIndent = (before.fixedIndent + before.hangIndent +
                       before.otherIndent)

        self.indentBegins = indentBegins
        self.fixedIndent = fixedIndent
        self.more = None
        self.prev = None
        self.pointTracker = pointTracker


    def islist(self):
        """
        It's not a list.
        """
        return False


    def connect(self, more):
        self.more = more
        more.prev = self
        return more


    @property
    def originalIndent(self):
        return self.indentBegins


    @property
    def words(self):
        """
        Used by wrapper below to see if there are any words in a given
        paragraph and whether it should be skipped.
        """
        return bool(self.lines)


    def matchesTag(self, other):
        return False


    def add(self, line):
        actualLine = self.pointTracker.peek(line)

        if actualLine.strip():
            if len(actualLine) - len(actualLine.lstrip()) <= self.indentBegins:
                next = self.connect(self.before.genRegular())
                return next.add(line)
            self.lines.append(line.rstrip())
        else:
            self.lines.append(line.strip())
        return self


    def fixIndentation(self):
        while self.lines and not self.lines[0].strip():
            self.lines.pop(0)
        while self.lines and not self.lines[-1].strip():
            self.lines.pop()
        if not self.lines:
            return
        cleanLines = list(map(self.pointTracker.peek, self.lines))
        commonLeadingIndent = min([len(x) - len(x.lstrip()) for x in cleanLines
                                   if x.strip()] or [0])
        newLines = []
        for actualLine, line in zip(cleanLines, self.lines):
            if actualLine != line and line[:commonLeadingIndent].strip():
                # There's a marker, and it's in the leading whitespace.
                # Explicitly reposition the marker at the beginning of the
                # fixed indentation.
                line = (self.pointTracker.marker +
                        actualLine[commonLeadingIndent:])
            else:
                line = line.rstrip()[commonLeadingIndent:]
            newLines.append(line)
        self.lines = newLines


    def wrap(self, output, indentation, width, initialBlank, singleSpace):
        # OK, now we know about all the lines we're going to know about.
        self.fixIndentation()
        for line in self.lines:
            if self.pointTracker.peek(line):
                output.write(indentation + "    " + self.fixedIndent)
            output.write(self.pointTracker.scan(line, output.tell()))
            output.write("\n")



class PointTracker(object):
    """
    Object for keeping track of where the insertion points are.
    """

    def __init__(self, point):
        self.point = point
        self.marker = "{" + makeID() + "}"
        self.outPoints = []


    def annotate(self, text):
        """
        Add point references to a block of text.
        """
        return text[:self.point] + self.marker + text[self.point:]


    def filterWords(self, words):
        for word in words:
            if self.isWord(word):
                yield self.peek(word)


    def isWord(self, text):
        """
        Is the given word actually a word, or just an artifact of the
        point-tracking process?  If it's just the point marker by itself, then
        no, it isn't, and don't insert additional whitespace after it.
        """
        return not (text == self.marker)


    def lengthOf(self, word):
        """
        How long would this word be if it didn't have any point-markers in it?
        """
        return len(self.peek(word))


    def peek(self, word):
        """
        What would this word look like if it didn't have any point-markers in
        it?
        """
        return word.replace(self.marker, "")


    def extractPoints(self, text):
        """
        Return a C{list} of all point markers contained in the text.
        """
        if self.marker in text:
            return [self.marker]
        return []


    def scan(self, text, offset):
        """
        Scan some text for point markers, remember them, and remove them.
        """
        idx = text.find(self.marker)
        if idx == -1:
            return text
        self.outPoints.append(idx + offset)
        return self.peek(text)



def wrapPythonDocstring(docstring, output, indentation="    ",
                        width=79, point=0, initialBlank=True,
                        singleSpace=False):
    """
    Wrap a given Python docstring.

    @param docstring: the docstring itself (just the stuff between the quotes).
    @type docstring: unicode

    @param output: The unicode output file to write the wrapped docstring to.
    @type output: L{file}-like (C{write} takes unicode.)

    @param indentation: a string (consisting only of spaces) indicating the
        amount of space to shift by.  Don't adjust this.  It's always 4 spaces.
        PEP8 says so.
    @type indentation: L{unicode}

    @param width: The maximum number of characters allowed in a wrapped line.
    @type width: L{int}

    @param point: The location of the cursor in the text, as an offset from the
        beginning of the docstring.  If this function is being used from within
        a graphical editor, this parameter can be used (in addition to the
        return value of this function) to reposition the cursor at the relative
        position which the user will expect.

    @param singleSpace: If true, use a single space between sentences instead
        of two.

    @return: The new location of the cursor.
    """
    # TODO: multiple points; usable, for example, for start and end of a
    # currently active selection.
    pt = PointTracker(point)
    start = paragraph = RegularParagraph(pt)
    docstring = pt.annotate(docstring)
    for line in docstring.split("\n"):
        paragraph = paragraph.add(line)
    prevp = None
    # output.write("{}".format(initialBlank))
    for paragraph in start.all():
        if initialBlank:
            if paragraph.words and not paragraph.matchesTag(prevp):
                output.write("\n")
        prevp = paragraph
        paragraph.wrap(output, indentation, width, initialBlank, singleSpace)
        initialBlank = True
    output.write(indentation)
    return pt.outPoints[0] if pt.outPoints else 0



def indentHeuristic(lines, io):
    """
    Determine the indentation.
    """
    for num, line in enumerate(lines):
        if num == 0:
            initialBlank = not bool(line)
            if not initialBlank:
                continue
        indentation = (len(line) - len(line.lstrip()))
        if indentation:
            return (initialBlank, indentation)
    # TODO: investigate the case where this happens.
    return True, 0



def sampleDocstring():
    """This is a sample docstring where the last word is a little bit too long
    go go.

    This is another part of the docstring.
    """


def main(argv, indata):
    parser = argparse.ArgumentParser()
    parser.add_argument("--offset", type = int)
    parser.add_argument("--indent", type = int)
    parser.add_argument("--width", type = int, default = 79)
    parser.add_argument("--linewise", action='store_true')
    parser.add_argument("--single-space", action='store_false')
    namespace = parser.parse_args(argv[1:])

    io = StringIO()
    inlines = indata.split("\n")
    if namespace.linewise:
        inlines.insert(0, "")
    initialBlank, indentCount = indentHeuristic(inlines, io)
    point = 0
    width = namespace.width

    if namespace.offset is not None:
        point = namespace.offset
    if namespace.indent is not None:
        indentCount = namespace.indent

    offset = wrapPythonDocstring(
        indata, io,
        indentation=" " * indentCount,
        width=width,
        point=point,
        initialBlank=initialBlank,
        singleSpace=namespace.single_space
    )
    prefix = StringIO()
    if namespace.offset is not None:
        prefix.write("{:d}".format(offset))
        if namespace.linewise:
            prefix.write("\n")
        else:
            prefix.write(" ")


    output = io.getvalue()
    if namespace.linewise:
        output = "\n".join(output.split("\n")[1:-1])
    output = prefix.getvalue() + output
    return output


if __name__ == '__main__':
    sys.stdout.write(
        toStdout(
            main(
                sys.argv,
                fromStdin(sys.stdin.read()),
            )
        )
    )

    sys.stdout.flush()
