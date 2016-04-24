#!/usr/bin/env python
from clipperz import app, db


def main():
    db.create_all()
    app.run(debug=True)

if __name__ == "__main__":
    main()
