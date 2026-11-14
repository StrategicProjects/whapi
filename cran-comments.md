## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a resubmission following CRAN feedback. The following issues were addressed:


### Found the following (possibly) invalid URLs: URL: https://gate.whapi.cloud
       
Response: Fixed. 

### It seems like you have too many spaces in your description field. Probably because linebreaks count as spaces too. Please remove unecassary ones.

Response: Fixed. Extra spaces and line breaks were removed.

### Please add a web reference for the API in the form <https:.....> to the description of the DESCRIPTION file with no space after 'https:' and angle brackets for auto-linking.

Response: Added the official API reference in the required format.

### Some code lines in examples are commented out. Please never do that. Ideally find toy examples that can be regularly executed and checked. Lengthy examples (> 5 sec), can be wrapped in \donttest{}.

Response: Commented code was removed. Toy examples were provided, and examples requiring an API token were wrapped in \dontrun{}.


