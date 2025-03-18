Development setup:

1. Make sure the "nodejs" package is installed on your system such
  that `node --version` in your command-line will return a version
  (OS-specific)

2. Change directories into this directory in your command-line and
   issue the command:

  `npm install`

3. Confirm the correct command-line for builds in `package.json`.

4. Start the running (& watching) build process with

   `npm run build`

5. Now your deployable css will be updated every time you trouch a
   file in your project. Keep an eye on the terminal where the build
   is running for any syntax errors (probably coming from your base
   css file in this directory, named with `-i` in the `package.json`
   but probably, <your-app>-base.css.




