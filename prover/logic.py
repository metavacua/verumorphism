# prover/logic.py

"""
This file contains the core logic for the prover, ported from ProverV0.lisp.
"""

from logic_core.formulas import formula_type, formula_arguments

class Prover:
    """
    A class to encapsulate the state and logic of the proof process.
    This follows the same improved, object-oriented design as the Refuter class.
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
        Public entry point to run the prover on a formula.
        """
        self.reset_counters()
        return self._run_recursive(formula)

    def _run_recursive(self, formula):
        """
        The main recursive dispatcher for the proof process.
        It tries axioms first, then rules, in a specific order.
        """
        # 1. Try axiom ConR
        axiom_result = self._axiom_conr(formula)
        if axiom_result is not None:
            return axiom_result

        # The original Lisp code tried rules in a specific order. We will do the same.
        # We check the formula type to decide which rule to try.
        ftype = formula_type(formula)

        if ftype == 'dep':
            # 2. Try rule *DEP*R
            dep_result = self._rule_dependence_r(formula)
            if dep_result is not None:
                return dep_result

        if ftype == 'ind':
            # 3. Try rule *IND*R
            ind_result = self._rule_independence_r(formula)
            if ind_result is not None:
                return ind_result

        # 4. No proof found
        return None

    def _axiom_conr(self, formula):
        """
        Proof Axiom (ConR): Proves (dep A A), returning A.
        """
        if formula_type(formula) == 'dep':
            args = formula_arguments(formula)
            if len(args) == 2 and args[0] == args[1]:
                self.axiom_applications += 1
                # The axiom proves the formula, returning the sub-formula 'A'
                return args[0]
        return None

    def _rule_dependence_r(self, formula):
        """
        Dependence Right Rule: Proves (dep A B) if both A and B are proven. (AND logic)
        """
        self.rule_applications += 1
        args = formula_arguments(formula)
        formula1 = args[0]
        formula2 = args[1]

        proof1_result = self._run_recursive(formula1)
        proof2_result = self._run_recursive(formula2)

        if proof1_result is not None and proof2_result is not None:
            # The rule proves the original formula
            return formula

        return None

    def _rule_independence_r(self, formula):
        """
        Independence Right Rule: Proves (ind A B) if either A or B is proven. (OR logic)
        """
        self.rule_applications += 1
        args = formula_arguments(formula)
        formula1 = args[0]
        formula2 = args[1]

        proof1_result = self._run_recursive(formula1)
        if proof1_result is not None:
            return formula

        proof2_result = self._run_recursive(formula2)
        if proof2_result is not None:
            return formula

        return None