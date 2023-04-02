# Team Details:
- **Anushka Panda**: 
    - Roll no.: **200174**
    - Phone no.: **+919929942481**
    - Email address: **apanda20@iitk.ac.in**
- **Mohil**: 
    - Roll no.: **200596**
    - Phone no.: **+918295534337**
    - Email address: **mohil20@iitk.ac.in**
- **Anshu Kumari**: 
    - Roll no.: **200150**
    - Phone no.: **+917742386923**
    - Email address: **anshu20@iitk.ac.in**

# Milestone 1
Uploaded to Canvas
# Milestone 2
## Bugs fixed:
- The GlobalTable class had an attribute called ```methods``` that was of the datatype ```vector< map< int, ID > >```. However, the Global Tables being initialised in the ```java.y``` file require just a ```map< int, ID >```. Hence in the file ```./src/classes/globalTable.h```,  line no. ```7```, ```methods``` was reassigned to a datatype of ```map< int, ID >```.
- A semicolon is missing in the initialisation ```vector<string> names``` ( line no. ```1896``` ). That was fixed.
- Some dead code that generated a ```.dot``` file for milestone 1 was deleted.
- Testcases were added.

## How to run?
The code must be run in the ```src``` folder. \
Once the Makefile is run (please refer steps 1-4 from ```Instructions Milestone 2.pdf```), all one needs to do is put the following command. 
```
java_symbol_table "/path/to/testcase.java"
```
For using our testcases, the path becomes `../tests/test_x.java`, where x ∊ [1, 10].