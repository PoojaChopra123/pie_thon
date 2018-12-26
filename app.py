#################################################
# Dependencies
#################################################
from flask import Flask, render_template, redirect, jsonify

#################################################
# Flask Setup
#################################################
app = Flask(__name__, static_url_path='/static')


#################################################
# Database Setup
#################################################


#################################################
# Flask Routes
#################################################
@app.route('/')
def home():
    """Return the homepage."""
    return render_template("index.html")

if __name__ == "__main__":
    app.run(debug=True)