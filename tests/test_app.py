# tests/test_app.py

import unittest
import json
from app import app

class TestApp(unittest.TestCase):

    def setUp(self):
        """Set up a test client for the Flask app."""
        self.app = app.test_client()
        self.app.testing = True

    def test_refute_success_refutable(self):
        """Test the /refute endpoint with a formula that should be refuted."""
        payload = {"formula": "('incon',)"}
        response = self.app.post('/refute',
                                 data=json.dumps(payload),
                                 content_type='application/json')

        self.assertEqual(response.status_code, 200)
        data = json.loads(response.get_data(as_text=True))
        self.assertEqual(data['status'], 'success')
        self.assertTrue(data['refuted'])
        self.assertEqual(data['refuted_formula'], "('incon',)")

    def test_refute_success_non_refutable(self):
        """Test the /refute endpoint with a formula that should not be refuted."""
        payload = {"formula": "('dual', ('incon',))"}
        response = self.app.post('/refute',
                                 data=json.dumps(payload),
                                 content_type='application/json')

        self.assertEqual(response.status_code, 200)
        data = json.loads(response.get_data(as_text=True))
        self.assertEqual(data['status'], 'success')
        self.assertFalse(data['refuted'])
        self.assertIsNone(data['non_refuted_state'])

    def test_refute_missing_formula_key(self):
        """Test the endpoint returns a 400 error if the 'formula' key is missing."""
        payload = {"data": "some other key"}
        response = self.app.post('/refute',
                                 data=json.dumps(payload),
                                 content_type='application/json')

        self.assertEqual(response.status_code, 400)
        data = json.loads(response.get_data(as_text=True))
        self.assertEqual(data['status'], 'error')
        self.assertIn("Missing 'formula' key", data['message'])

    def test_refute_malformed_formula(self):
        """Test the endpoint returns a 400 error for a malformed formula string."""
        payload = {"formula": "('ind', 'a'"} # Missing closing parenthesis
        response = self.app.post('/refute',
                                 data=json.dumps(payload),
                                 content_type='application/json')

        self.assertEqual(response.status_code, 400)
        data = json.loads(response.get_data(as_text=True))
        self.assertEqual(data['status'], 'error')
        self.assertIn("Invalid or malformed formula format", data['message'])

    def test_refute_not_json(self):
        """Test the endpoint returns a 400 error if the request is not JSON."""
        response = self.app.post('/refute',
                                 data="this is not json",
                                 content_type='text/plain')

        self.assertEqual(response.status_code, 400)
        data = json.loads(response.get_data(as_text=True))
        self.assertEqual(data['status'], 'error')
        self.assertIn("Request must be JSON", data['message'])

if __name__ == '__main__':
    unittest.main()