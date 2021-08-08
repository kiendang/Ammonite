# https://github.com/python/cpython/
# blob/a40675c659cd8c0699f85ee9ac31660f93f8c2f5/Lib/rlcompleter.py#L137
import inspect as __scalapy_completion_inspect


def __scalapy_completion_attr_matches(thisobject, attr):
    words = set(dir(thisobject))
    words.discard("__builtins__")

    if hasattr(thisobject, '__class__'):
        words.add('__class__')
        words.update(__scalapy_completion_get_class_members(thisobject.__class__))
    matches = []
    n = len(attr)
    if attr == '':
        noprefix = '_'
    elif attr == '_':
        noprefix = '__'
    else:
        noprefix = None
    while True:
        for word in words:
            if (word[:n] == attr and
                not (noprefix and word[:n+1] == noprefix)):
                match = word
                if isinstance(getattr(type(thisobject), word, None),
                                property):
                    matches.append(match)
                    continue
                if (value := getattr(thisobject, word, None)) is not None:
                    matches.append(__scalapy_completion_callable_postfix(value, match))
                else:
                    matches.append(match)
        if matches or not noprefix:
            break
        if noprefix == '_':
            noprefix = '__'
        else:
            noprefix = None
    matches.sort()
    return matches


def __scalapy_completion_get_class_members(klass):
    ret = dir(klass)
    if hasattr(klass,'__bases__'):
        for base in klass.__bases__:
            ret = ret + __scalapy_completion_get_class_members(base)
    return ret


def __scalapy_completion_callable_postfix(val, word):
    if callable(val):
        word += "("
        try:
            if not __scalapy_completion_inspect.signature(val).parameters:
                word += ")"
        except ValueError:
            pass

    return word
