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

# --- Core Refuter Logic ---

class Refuter:
    """
    A class to encapsulate the state and logic of the refutation process.
    This fixes a bug in the original Lisp code where complexity counters
    were reset on every recursive call.
    """
    def __init__(self):
        self.axiom_applications = 0
        self.rule_applications = 0

    def reset_counters(self):
        """Resets the complexity counters."""
        self.axiom_applications = 0
        self.rule_applications = 0

    def run(self, formula):
        """
        Public entry point to run the refuter on a formula.
        It resets the counters and starts the recursive refutation process.
        """
        self.reset_counters()
        return self._run_recursive(formula)

    def _run_recursive(self, formula):
        """
        The main recursive dispatcher. It checks the formula type and
        calls the appropriate axiom or rule. This is the function that
        rules call for recursion, ensuring counters are not reset.
        """
        # Axiom check first, as it's a terminal condition.
        axiom_result = self._axiom_inconl(formula)
        if axiom_result is not None:
            return axiom_result

        # Dispatch to the appropriate rule based on formula type.
        ftype = formula_type(formula)
        if ftype == 'ind':
            return self._rule_independence_l(formula)
        elif ftype == 'dep':
            return self._rule_dependence_l(formula)
        elif ftype == 'dual':
            return self._rule_dual_l(formula)

        return None

    def _axiom_inconl(self, formula):
        """
        Axiom InconL: Refutes (ind A A) and (incon).
        """
        ftype = formula_type(formula)

        if ftype == 'incon':
            self.axiom_applications += 1
            return formula

        if ftype == 'ind':
            args = formula_arguments(formula)
            if len(args) == 2 and args[0] == args[1]:
                self.axiom_applications += 1
                return formula

        return None

    def _rule_dual_l(self, formula):
        """
        Rule dualL: Refutes (dual A) if A is refuted.
        Also handles the (dual (dual A)) and (dual incon) cases.
        """
        self.rule_applications += 1

        inner_formula = formula_arguments(formula)[0]

        # Check for (dual (dual A)) simplification.
        if formula_type(inner_formula) == 'dual':
            simplified_formula = formula_arguments(inner_formula)[0]
            return self._run_recursive(simplified_formula)

        # Check for (dual incon) case, which is not refuted.
        if formula_type(inner_formula) == 'incon':
            return None

        # Standard dualL rule: refute (dual A) if A is refuted.
        refutation_result = self._run_recursive(inner_formula)
        if refutation_result:
            return formula

        return None

    def _rule_dependence_l(self, formula):
        """
        Dependence Left Rule: Refutes (dep A B) if both A and B are refuted. (AND logic)
        """
        self.rule_applications += 1
        args = formula_arguments(formula)
        formula1 = args[0]
        formula2 = args[1]

        refutation1_result = self._run_recursive(formula1)
        refutation2_result = self._run_recursive(formula2)

        if refutation1_result and refutation2_result:
            return formula

        return None

    def _rule_independence_l(self, formula):
        """
        Independence Left Rule: Refutes (ind A B) if either A or B is refuted. (OR logic)
        """
        self.rule_applications += 1
        args = formula_arguments(formula)
        formula1 = args[0]
        formula2 = args[1]

        refutation1_result = self._run_recursive(formula1)
        if refutation1_result:
            return formula

        refutation2_result = self._run_recursive(formula2)
        if refutation2_result:
            return formula

        return None