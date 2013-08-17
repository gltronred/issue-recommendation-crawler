issue-recommendation-crawler
============================


How to install and run
======================

Build: 

````
cabal install
````

Run:

````
issue-recommendation-crawler <file-with-projects> [Print|Solr <address>]
````

File with projects has format:

````
# comments are written after # if needed
owner1 project1
owner2 project2
...
ownerN projectN
````

JSON output
===========

````json

{
  "issue": {
    "owner": "gltronred",
    "repo": "issue-recommendation-crawler",
    "id": 1 },
  "languages": {
    "haskell": 95.1,
    "bash": 4.8,
    "unknown": 0.1 },
  "frameworks": [
    "aeson",
    "github" ],
  "size": 40,
  "stars": 5,
  "watches": 40,
  "discussion": 100500,
  "quality": 99.9,
  "due": "2013-08-18T16:00:55",
  "discusses": 10,
  "tags": [
    "bug",
    "ui",
    "@high"]
}  

````

* @issue -- describes issue address (<owner>/<project>/issues/<id>)
* @languages
* @frameworks
* @size
* @stars
* @watches
* @discussion
* @quality
* @due
* @discusses
* @tags