# tests/test_prover.py

import unittest
from prover.logic import Prover
from logic_core.formulas import make_con, make_dep, make_ind

class TestProver(unittest.TestCase):

    def setUp(self):
        """Set up a new Prover instance for each test."""
        self.prover = Prover()
        self.con = make_con()
        self.atom_a = 'a'

    def test_axiom_conr(self):
        """Test the _axiom_conr method directly."""
        # Axiom should prove (dep A A) and return A
        formula = make_dep(self.atom_a, self.atom_a)
        result = self.prover._axiom_conr(formula)
        self.assertEqual(result, self.atom_a)
        self.assertEqual(self.prover.axiom_applications, 1)

    def test_axiom_conr_fails(self):
        """Test that the axiom does not apply to other formulas."""
        # Should not apply to (dep A B)
        formula1 = make_dep(self.atom_a, 'b')
        self.assertIsNone(self.prover._axiom_conr(formula1))

        # Should not apply to (ind A A)
        formula2 = make_ind(self.atom_a, self.atom_a)
        self.assertIsNone(self.prover._axiom_conr(formula2))

    def test_run_on_simple_con(self):
        """Test run cannot prove a simple ('con') formula."""
        result = self.prover.run(self.con)
        self.assertIsNone(result)
        self.assertEqual(self.prover.axiom_applications, 0)
        self.assertEqual(self.prover.rule_applications, 0)

    def test_run_on_dep_A_A_is_axiom(self):
        """Test run proves (dep A A) via axiom ConR."""
        formula = make_dep(self.con, self.con)
        result = self.prover.run(formula)
        # The result of the proof is the sub-formula 'A'
        self.assertEqual(result, self.con)
        self.assertEqual(self.prover.axiom_applications, 1)
        self.assertEqual(self.prover.rule_applications, 0)

    def test_run_on_ind_con_con(self):
        """Test run cannot prove (ind (con) (con))."""
        # Neither sub-formula is provable, so the 'ind' rule fails.
        formula = make_ind(self.con, self.con)
        result = self.prover.run(formula)
        self.assertIsNone(result)
        # 1 for the rule, 0 for axioms, 0 for sub-rules
        self.assertEqual(self.prover.rule_applications, 1)
        self.assertEqual(self.prover.axiom_applications, 0)

    def test_run_on_dep_of_provable(self):
        """Test proves (dep (dep A A) (dep B B))."""
        # Both sub-formulas are provable by the axiom.
        part1 = make_dep(self.atom_a, self.atom_a)
        part2 = make_dep('b', 'b')
        formula = make_dep(part1, part2)
        result = self.prover.run(formula)
        self.assertEqual(result, formula)
        # 1 for the outer 'dep' rule, 2 for the inner axioms.
        self.assertEqual(self.prover.rule_applications, 1)
        self.assertEqual(self.prover.axiom_applications, 2)

    def test_run_on_ind_of_provable(self):
        """Test proves (ind (dep A A) (con))."""
        # The first sub-formula is provable, so the 'ind' rule short-circuits.
        part1 = make_dep(self.atom_a, self.atom_a)
        part2 = self.con
        formula = make_ind(part1, part2)
        result = self.prover.run(formula)
        self.assertEqual(result, formula)
        # 1 for the 'ind' rule, 1 for the axiom on the first part.
        self.assertEqual(self.prover.rule_applications, 1)
        self.assertEqual(self.prover.axiom_applications, 1)

if __name__ == '__main__':
    unittest.main()