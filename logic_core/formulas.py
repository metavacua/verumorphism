# logic_core/formulas.py

"""
This file contains the shared logic for creating and inspecting logical formulas.
It serves as the core data structure definition for both the prover and the refuter.
"""

# We represent Lisp S-expressions like '(con)' or '(dep a b)' as Python tuples.
# e.g., ('con',), ('dep', a, b)

def make_con():
    """Creates a 'con' (consistent) formula."""
    return ('con',)

def make_incon():
    """Creates an 'incon' (inconsistent) formula."""
    return ('incon',)

def make_dep(formula1, formula2):
    """Creates a 'dep' (dependent) formula."""
    return ('dep', formula1, formula2)

def make_ind(formula1, formula2):
    """Creates an 'ind' (independent) formula."""
    return ('ind', formula1, formula2)

def make_dual(formula):
    """Creates a 'dual' formula."""
    return ('dual', formula)

def formula_type(formula):
    """Returns the type of a formula (e.g., 'con', 'dep')."""
    if isinstance(formula, tuple) and len(formula) > 0:
        return formula[0]
    return None

def formula_arguments(formula):
    """Returns the arguments of a formula."""
    if isinstance(formula, tuple) and len(formula) > 1:
        return formula[1:]
    return ()