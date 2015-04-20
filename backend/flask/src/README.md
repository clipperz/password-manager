clipperz
========
A flask based backend for the Clipperz (https://clipperz.is) Password Manager. This backend is for development and personal use only. As such it does not implement any bot protection mechanisms such as tolls.

Running
-------
Once you have built the backend using the clipperz build process you can use run.sh to create an environment for testing against. The database will be created in the target directory which means it will be over-ridden every time you build. To change this you can specify a `DATABASE_URL` environment variable that points to another location.

Once it is running, you can open the clipperz by going to `http://localhost:5000/<frontend version>` for instance (http://localhost:5000/delta)
