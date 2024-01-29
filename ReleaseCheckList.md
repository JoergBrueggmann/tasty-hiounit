# Release Check List
This check list is mandatory to be applied before *release* and *pull requests* respectively.

Go thru the check list, answer the question.
If there is any question resulting in 'NO' then correct.
After correction of all points repeat the complete procedure again, to avoid regressions.

## Versioning

Applicability: *upload to Hackage*, *release*

- Is version number in file 'package.yaml' set according to the rules stated in the beginning of file 'CHANGELOG.md'?
- Has the version an entry in file 'CHANGELOG.md' according to its rules?

## Code Review

Applicability: *upload to Hackage*, *release*, *pull request*

- Is there no code commented out that has also no TODO?
- Are all comments with TODOs valid and according to the intended change?

## Documentation Review

Applicability: *upload to Hackage*, *release*, *pull request*

- Are all changes reflected in documentation?
- Do have all modules a proper description?
- Do have all exported classes, functions and types a proper description?
- Do have all internal functions a proper description or at least a commment with a TODO?

## Test Review

Applicability: *upload to Hackage*, *release*, *pull request*

### Test of Test Functions
Functions that are functions to automate test will not be tested automatically by itself they will be tested manually.

- Are all functions to automate test show negative result when they should?

    - Therefore, test manually:

        - For each test function, build equivalence classes of parameters, change the parameters accordingly to verify whether test results are always valid.

### Test of Changes

- Are all changes reflected in tests?

### Tests of Functions

- Are all the tests of functions commented in the following form:

        {-  * validated: ✅
                * completeness: ✅
                * independence: ✅
                * edge cases  : ✅
                * conform doc.: ✅ -}

- Do the comments reflect the truth according to the criteria below?

    - completeness:

        - the complete set of possible test values/data is used in unit tests,
        or values for all possible equivalence classes are used
        or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases (QuickCheck)

    - independence:
        - test method is independent from library functions that are under test,
        or dependent on tested functions

    - edge cases:
        - edge cases are tested by unit tests (HUnit)

    - conform doc.:
        - all tests are conform to documentation, e.g. source code comments by haddock)

### Tests of Classes

- Are all the tests of classes commented in the following form:

        {-  * validated: ✅
                * documented: ✅
                * laws             : ✅
                * characteristics  : ✅
                * prefix           : ✅
                * short description: ✅
                * completeness: ✅
                    * all laws documented          : ✅
                    * all type combinations        : ✅
                    * stubbed for default functions: ✅ -}

- Do the comments reflect the truth according to the criteria below?

    - documented:
        - laws:
            laws and its criteria are described and are verifyable
        - characteristics:
            the general characteristics of the class are described
        - prefix
            the prefix of the class is given
        - short description
            a short description is given
    - completeness:
        - the class is tested to fulfill all laws
        - the class is tested for all types and type combinations, respectively
        and the class is tested for all types and type combinations
    - stubbed:
        if any, the default functions of the class are also tested by stub instance(s), as well

### Tests of Modules

- Are all the tests of modules commented in the following form:

        {-  * validated: ✅
                * documented: ✅
                * completeness: ✅ -}

- Do the comments reflect the truth according to the criteria below?

    - documented:
        there is completely and correct filled header like:

            {-|
            Description : ... convinient functions to use the GHC API.
            Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
            License     : BSD-3-Clause
            Maintainer  : info@joerg-brueggmann.de
            Stability   : experimental
            Portability : POSIX

            The module 'Test.Tasty.Internal.GhcApiWrap' provides helper functions to use the GHC API more conviniently.

            Suggested import line: 'import qualified Test.Tasty.Internal.GhcApiWrap as Ghc'

            ...

    - It follows a general description of the purpose of the module.

    - completeness:
        - all classes are in the root test group
        - all global functions have tests in the root test group
        - all classes are validated
        - all global functions are validated

## Re-Build All

Applicability: *upload to Hackage*, *release*, *pull request*

- Delete build and test output completely

    rm -r ./.iotest/
    rm -r ./.stack-work

- Build and install software

    `stack build`

    `stack install`

    `stack test`

    `stack haddock`

- Is there NO warning or error during build process?
- Is there NO warning or error during install process?
- Is there NO warning or error during test build process?
- Are all tests successful?
- Is there NO warning or error during documentation build process (*haddock*)?

## Before Upload to Hackage

Applicability: *upload to Hackage*

- When executing `cabal check` does it display the following output?

        No errors or warnings could be found in the package.

- When executing `cabal gen-bounds` does it display the following output?

        Congratulations, all your dependencies have upper bounds!

    - If no, update section 'dependencies:' in file 'package.yaml' accordingly

- When executing `stack sdist` does it display no warning or errors and is the tarball file available?

# Only when all the steps before are successful

Applicability: *upload to Hackage*

- Upload the tarball file as *Package candidate* here: https://hackage.haskell.org/packages/candidates/upload

- When the prior step was successful, upload the tarball file as *Package candidate* here: https://hackage.haskell.org/packages/upload

- In case the documentation is not been properly available, execute the following commands:

        $ cabal haddock --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' --haddock-hyperlink-source --haddock-quickjump --haddock-for-hackage
        $ cabal upload -d  --publish <PathToTarbalFile>.tar.gz

- Check the result of the build that Hackage attempts to do on following address https://matrix.hackage.haskell.org/package/hiounit
