# refuter/logic.py

"""
This file contains the core logic for the refuter, ported from the original Lisp codebase.
"""

# --- Formula Representation ---
# We represent Lisp S-expressions like '(incon)' or '(dep a b)' as Python tuples.
# e.g., ('incon',), ('dep', a, b)

def make_incon():
    """Creates an 'incon' formula."""
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
    """Returns the type of a formula (e.g., 'incon', 'dep')."""
    if isinstance(formula, tuple) and len(formula) > 0:
        return formula[0]
    return None

def formula_arguments(formula):
    """Returns the arguments of a formula."""
    if isinstance(formula, tuple) and len(formula) > 1:
        return formula[1:]
    return ()

# --- Axioms ---

def axiom_inconl(formula):
    """
    Python port of the 'axiom-inconl' Lisp function.

    Axiom InconL: Refutes (ind A A) and (incon) as the base cases for refutation.
    - If the formula is ('incon',), it is refuted.
    - If the formula is ('ind', A, A) where the two arguments are equal, it is refuted.
    - Otherwise, the axiom does not apply.

    Returns:
        The formula if it is refuted by this axiom, otherwise None.
    """
    ftype = formula_type(formula)

    # Case 1: Refutes (incon)
    if ftype == 'incon':
        return formula

    # Case 2: Refutes (ind A A)
    if ftype == 'ind':
        args = formula_arguments(formula)
        if len(args) == 2 and args[0] == args[1]:
            return formula

    # Axiom does not apply
    return None