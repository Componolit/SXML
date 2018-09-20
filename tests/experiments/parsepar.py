#!/usr/bin/env python3

import unittest

def parse_open (data, offset):

    if offset < len (data) and data[offset] == "(":
        return (True, offset + 1)
    return (False, offset)

def parse_close (data, offset):

    if offset < len (data) and data[offset] == ")":
        return (True, offset + 1)
    return (False, offset)

def parse_recursive (data, offset = 0):

    (found, offset) = parse_open (data, offset)
    if not found:
        return (False, offset)

    while found:
        (found, offset) = parse_recursive (data, offset)

    (found, offset) = parse_close (data, offset)
    if not found:
        return (False, offset)

    return (True, offset)

def parse_iterative (data, offset = 0):

    found = False
    result = (False, 0)

    stack = []
    stack.append ({'offset': offset, 'found': found, 'location': 0})

    while len (stack) > 0:

        current = stack.pop ()

        if current['location'] == 0:

            (found, offset) = parse_open (data, current['offset'])
            if not found:
                result = (False, offset)
            else:
                stack.append ({'offset': offset, 'found': found, 'location': 1})

        if current['location'] == 1:

            (found, offset) = parse_recursive (data, current['offset'])
            if found:
                stack.append ({'offset': offset, 'found': found, 'location': 1})
            else:
                stack.append ({'offset': offset, 'found': found, 'location': 2})
            stack.append ({'offset': offset, 'found': found, 'location': 0})

        if current['location'] == 2:

            (found, offset) = parse_close (data, current['offset'])
            if not found:
                result = (False, offset)
            else:
                stack.append ({'offset': offset, 'found': found, 'location': 3})

        if current['location'] == 3:
            result = (True, offset)

    return result

class TestParser(unittest.TestCase):

    def test_simple1(self):
        self.assertEqual (parse_recursive ("()"), (True, 2))

    def test_simple2(self):
        self.assertEqual (parse_recursive ("(())"), (True, 4))

    def test_simple3(self):
        self.assertEqual (parse_recursive ("(()())"), (True, 6))

    def test_simple9(self):
        self.assertEqual (parse_recursive ("(()()()(((((()))))()))"), (True, 22))

    def test_fail(self):
        self.assertEqual (parse_recursive ("(()(((((()))))())("), (False, 18))

    def test_fail_short1(self):
        self.assertEqual (parse_recursive ("("), (False, 1))

    def test_fail_short2(self):
        self.assertEqual (parse_recursive (")"), (False, 0))

    def test_fail_unrelated(self):
        self.assertEqual (parse_recursive ("This is a test"), (False, 0))

    def test_simple1_i(self):
        self.assertEqual (parse_iterative ("()"), (True, 2))

    def test_simple2_i(self):
        self.assertEqual (parse_iterative ("(())"), (True, 4))

    def test_simple3_i(self):
        self.assertEqual (parse_iterative ("(()())"), (True, 6))

    def test_simple9_i(self):
        self.assertEqual (parse_iterative ("(()()()(((((()))))()))"), (True, 22))

    def test_fail_i(self):
        self.assertEqual (parse_iterative ("(()(((((()))))())("), (False, 18))

    def test_fail_short1_i(self):
        self.assertEqual (parse_iterative ("("), (False, 1))

    def test_fail_short2_i(self):
        self.assertEqual (parse_iterative (")"), (False, 0))

    def test_fail_unrelated_i(self):
        self.assertEqual (parse_iterative ("This is a test"), (False, 0))

if __name__ == '__main__':
    unittest.main()
