# tests/test_logic.py

import unittest
from refuter.logic import Refuter, make_incon, make_ind, make_dep, make_dual

class TestRefuter(unittest.TestCase):

    def setUp(self):
        """Set up a new Refuter instance for each test."""
        self.refuter = Refuter()
        self.incon = make_incon()
        self.atom_a = 'a'
        self.atom_b = 'b'

    def test_axiom_inconl(self):
        """Test the _axiom_inconl method directly."""
        # Test 1: Should refute (incon)
        formula1 = self.incon
        self.assertEqual(formula1, self.refuter._axiom_inconl(formula1))

        # Test 2: Should refute (ind A A)
        formula2 = make_ind(self.atom_a, self.atom_a)
        self.assertEqual(formula2, self.refuter._axiom_inconl(formula2))

        # Test 3: Should NOT refute (ind A B)
        formula3 = make_ind(self.atom_a, self.atom_b)
        self.assertIsNone(self.refuter._axiom_inconl(formula3))

    def test_run_on_simple_incon(self):
        """Test run refutes a simple ('incon') formula."""
        result = self.refuter.run(self.incon)
        self.assertEqual(result, self.incon)
        self.assertEqual(self.refuter.axiom_applications, 1)
        self.assertEqual(self.refuter.rule_applications, 0)

    def test_run_on_dep_incon_incon(self):
        """Test run refutes (dep incon incon). Should succeed."""
        formula = make_dep(self.incon, self.incon)
        result = self.refuter.run(formula)
        self.assertEqual(result, formula)
        # 1 for the rule, 1 for each sub-formula axiom
        self.assertEqual(self.refuter.rule_applications, 1)
        self.assertEqual(self.refuter.axiom_applications, 2)

    def test_run_on_ind_A_A_is_axiom(self):
        """Test run refutes (ind A A) as an AXIOM, not a rule."""
        formula = make_ind(self.incon, self.incon)
        result = self.refuter.run(formula)
        self.assertEqual(result, formula)
        # This is an axiom case, so no rules should be applied.
        self.assertEqual(self.refuter.rule_applications, 0)
        self.assertEqual(self.refuter.axiom_applications, 1)

    def test_run_on_ind_rule_short_circuit(self):
        """Test the 'ind' rule with a formula that is not an axiom."""
        # F = (ind (incon) (dep a b))
        # The first part is refutable, so the rule should succeed and short-circuit.
        part1 = self.incon
        part2 = make_dep(self.atom_a, self.atom_b)
        formula = make_ind(part1, part2)
        result = self.refuter.run(formula)
        self.assertEqual(result, formula)
        # 1 for the 'ind' rule, 1 for the axiom on the first part.
        self.assertEqual(self.refuter.rule_applications, 1)
        self.assertEqual(self.refuter.axiom_applications, 1)

    def test_run_on_dual_incon(self):
        """Test run does NOT refute (dual incon)."""
        formula = make_dual(self.incon)
        result = self.refuter.run(formula)
        self.assertIsNone(result)
        self.assertEqual(self.refuter.rule_applications, 1)
        self.assertEqual(self.refuter.axiom_applications, 0)

    def test_run_on_dual_dual_incon(self):
        """Test run refutes (dual (dual incon)) as it simplifies to (incon)."""
        formula = make_dual(make_dual(self.incon))
        result = self.refuter.run(formula)
        # The result should be the simplified formula, which is ('incon',)
        self.assertEqual(result, self.incon)
        # 1 for the dual rule, 1 for the final axiom application
        self.assertEqual(self.refuter.rule_applications, 1)
        self.assertEqual(self.refuter.axiom_applications, 1)

    def test_run_on_dual_of_refutable_formula(self):
        """Test run refutes (dual (dep incon incon))."""
        # (dep incon incon) is refutable
        refutable_inner = make_dep(self.incon, self.incon)
        formula = make_dual(refutable_inner)
        result = self.refuter.run(formula)
        self.assertEqual(result, formula)
        # 1 for the dual rule, 1 for the dep rule, 2 for the axioms
        self.assertEqual(self.refuter.rule_applications, 2)
        self.assertEqual(self.refuter.axiom_applications, 2)

    def test_run_on_complex_formula(self):
        """Test a more complex nested formula."""
        # F = (ind (dep incon incon) (ind incon incon))
        # The first part (dep incon incon) is refutable, so the whole thing is.
        part1 = make_dep(self.incon, self.incon)
        part2 = make_ind(self.incon, self.incon)
        formula = make_ind(part1, part2)
        result = self.refuter.run(formula)
        self.assertEqual(result, formula)
        # 1 for the outer 'ind', 1 for the inner 'dep', 2 for the axioms.
        # The second part of the 'ind' is not evaluated due to short-circuiting.
        self.assertEqual(self.refuter.rule_applications, 2)
        self.assertEqual(self.refuter.axiom_applications, 2)

if __name__ == '__main__':
    unittest.main()