# app.py

from flask import Flask, request, jsonify
from ast import literal_eval

from refuter.logic import Refuter

app = Flask(__name__)

# Instantiate the refuter once to be reused across requests
refuter = Refuter()

@app.route('/refute', methods=['POST'])
def refute_endpoint():
    """
    Handles refutation requests.
    Expects a JSON body with a 'formula' key.
    e.g., {"formula": "('ind', ('incon',), ('incon',))"}
    """
    if not request.is_json:
        return jsonify({"status": "error", "message": "Request must be JSON"}), 400

    data = request.get_json()
    formula_string = data.get('formula')

    if not formula_string:
        return jsonify({"status": "error", "message": "Missing 'formula' key in request body"}), 400

    try:
        # Safely evaluate the string representation of the formula tuple
        formula = literal_eval(formula_string)
    except (ValueError, SyntaxError):
        return jsonify({"status": "error", "message": f"Invalid or malformed formula format: {formula_string}"}), 400

    # Run the refuter logic
    result = refuter.run(formula)

    # Format the response
    if result is not None:
        response = {
            "status": "success",
            "refuted": True,
            "refuted_formula": str(result)
        }
    else:
        response = {
            "status": "success",
            "refuted": False,
            "non_refuted_state": None
        }

    return jsonify(response)

if __name__ == '__main__':
    # Note: This is for local development and debugging.
    # In a production environment, a proper WSGI server like Gunicorn would be used.
    app.run(debug=True, port=8080)