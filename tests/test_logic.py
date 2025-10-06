# tests/test_logic.py

import unittest
from refuter.logic import make_incon, make_ind, make_dep, make_dual, axiom_inconl

class TestAxiomInconl(unittest.TestCase):

    def test_refutes_incon(self):
        """
        Tests that the axiom correctly refutes a simple ('incon',) formula.
        """
        formula = make_incon()
        self.assertEqual(formula, axiom_inconl(formula), "Should refute ('incon',)")

    def test_refutes_ind_A_A(self):
        """
        Tests that the axiom correctly refutes an ('ind', A, A) formula
        where the arguments are identical.
        """
        atom_a = 'a'
        formula = make_ind(atom_a, atom_a)
        self.assertEqual(formula, axiom_inconl(formula), "Should refute ('ind', A, A)")

    def test_does_not_refute_ind_A_B(self):
        """
        Tests that the axiom does NOT refute an ('ind', A, B) formula
        where the arguments are different.
        """
        atom_a = 'a'
        atom_b = 'b'
        formula = make_ind(atom_a, atom_b)
        self.assertIsNone(axiom_inconl(formula), "Should not refute ('ind', A, B)")

    def test_does_not_refute_other_types(self):
        """
        Tests that the axiom does not apply to other formula types, like 'dep'.
        """
        formula = make_dep('a', 'b')
        self.assertIsNone(axiom_inconl(formula), "Should not apply to other formula types")

    def test_handles_complex_identical_arguments(self):
        """
        Tests that the axiom correctly identifies identical complex arguments
        in an 'ind' formula.
        """
        complex_arg = make_dep('a', make_dual('b'))
        formula = make_ind(complex_arg, complex_arg)
        self.assertEqual(formula, axiom_inconl(formula), "Should refute ('ind', X, X) where X is complex")

if __name__ == '__main__':
    unittest.main()