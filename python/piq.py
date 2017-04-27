#!/usr/bin/env python

# TODO
#
# - gen piq ast
# - type/typed
#
# piqi.py:
#
#   - gen-dsl subcommand that takes foo.piqi spec and generates foo_dsl.py
#   - dsl runtime


import sys
import os
import json


def exit_with_error(loc, s):
    sys.stderr.write("{}: piq error: {}\n".format(loc, s))
    sys.exit(1)


def is_piq_node(x):
    return isinstance(x, (Name, Named, List, Splice, Value))


# make piq AST node
def make_node(x, loc=None, is_inside_list=False):
    if is_piq_node(x):
        # already a Piq node
        return x
    elif isinstance(x, Keyword):
        return x.name
    elif isinstance(x, (bool, int, float, basestring)):
        return Value(x, loc)
    elif isinstance(x, list):
        # XXX: support iterables?
        items = [make_node(item, loc, is_inside_list=True) for item in x]
        return List(items, loc)
    else:
        if is_inside_list:
            error_prefix = "list element of invalid type"
        else:
            error_prefix = "value of invalid type"
        error = "{} '{}': {}".format(error_prefix, type_name(x), x)
        exit_with_error(loc, error)


def type_name(x):
    return type(x).__name__


# splice Splice'd values into the outer lists
def expand_slices(x):
    node = make_node(x)
    if isinstance(node, List):
        new_items = []
        for item in node.items:
            if isinstance(item, Splice):
                new_items.extend(item.expand())
            else:
                new_items.append(expand_slices(item))
        node.items = new_items

    return node



# value of one of the primitive Piq types
class Value(object):
    def __init__(self, value, loc):
        self.value = value
        self.loc = loc

    def __repr__(self):
        return repr(self.value)


# list of nodes
class List(object):
    def __init__(self, items, items_loc):
        self.items = items
        self.items_loc = items_loc

    def __repr__(self):
        return repr(self.items)


# list of values to be spliced into the containing list
class Splice(object):
    def __init__(self, name, items, items_loc):
        self.name = name
        self.items = items
        self.items_loc = items_loc

    def __repr__(self):
        return repr(self.name) + '* ' + repr(self.items)

    def expand(self):
        return [Named(self.name, x) for x in self.items]


class Name(object):
    def __init__(self, name, loc):
        self.name = name
        self.loc = loc

    def __repr__(self):
        return '.' + self.name


class Named(object):
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def __repr__(self):
        name_repr = repr(self.name)
        value_repr = repr(self.value)

        if isinstance(self.value, Name) or isinstance(self.value, Named):
            return name_repr + ' (' + value_repr + ')'
        else:
            return name_repr + ' ' + value_repr


class Loc(object):
    def __init__(self, init_loc):
        self.file, self.line, self.column = init_loc

    def __repr__(self):
        return "{}:{}:{}".format(self.file, self.line, self.column)


def make_loc(init_loc):
    if init_loc is None:
        return None
    else:
        return Loc(init_loc)


class Keyword(object):
    def __init__(self, name, name_loc, value_loc=None):
        self.name = Name(name, make_loc(name_loc))
        self.value_loc = make_loc(value_loc)

    def __repr__(self):
        return repr(self.name)

    def __pow__(self, other):
        node = make_node(other, self.value_loc)
        return Named(self.name, node)

    def __mul__(self, other):
        node = make_node(other, self.value_loc)

        if isinstance(node, List):
            return Splice(self.name, node.items, node.items_loc)
        else:
            exit_with_error(
                    node.loc,
                    "piq error: {}* must be followed by a list, instead followed by a {}".format(self.name, format_node_type(node))
            )


def format_node_type(x):
    if isinstance(x, Name):
        return "piq name"
    elif isinstance(x, Named):
        return "piq named value"
    if isinstance(x, List):
        return "piq list"
    elif isinstance(x, Splice):
        return "piq splice"
    elif isinstance(x, Value):
        return "value of type '{}'".format(type_name(x.value))
    else:
        assert False


def node_to_json_ast(x):
    def node_to_piq_loc_pair(x):
        if isinstance(x, Name):
            return dict(name=x.name), x.loc
        elif isinstance(x, Named):
            value = dict(name=x.name.name, value=node_to_json_ast(x.value))
            return dict(named=value), x.name.loc
        elif isinstance(x, List):
            items = [node_to_json_ast(item) for item in x.items]
            return dict(list=items), x.items_loc
        elif isinstance(x, Splice):
            items = [node_to_json_ast(item) for item in x.items]
            value = dict(name=x.name.name, item=items)
            return dict(splice=value), x.name.loc
        elif isinstance(x, Value):
            value = x.value
            if isinstance(value, int):
                # TODO: use uint when int is not wide enough
                piq = dict(int=value)
            elif isinstance(value, float):
                # TODO: make sure generated floats are reversible
                piq = dict(float=value)
            elif isinstance(value, bool):
                piq = dict(bool=value)
            elif isinstance(value, basestring):
                # TODO: binary, text, raw-string, word
                piq = dict(string=value)
            else:
                assert(False)
            return piq, x.loc
        else:
            assert False

    piq_ast, loc = node_to_piq_loc_pair(x)

    loc_ast = None
    if loc is not None:
        loc_ast = dict(file=loc.file, line=loc.line, column=loc.column)

    return dict(piq=piq_ast, loc=loc_ast)


def to_piq(x):
    return make_node(x)


def to_json_ast(x):
    node = make_node(x)
    return node_to_json_ast(node)


def to_json(x):
    return json.dumps(to_json_ast(x))


def main():
    pass


if __name__ == '__main__':
    main()
