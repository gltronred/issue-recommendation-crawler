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
  "id": "gltronred/issue-recommendation-crawler/1",
  "owner": "gltronred",
  "project": "issue-recommendation-crawler",
  "number": 1,
  "title": "Some Title, e.g. Lord",
  "body": "Fat body",
  "languages": [
    "haskell",
    "bash",
    "unknown"],
  "frameworks": [
    "aeson",
    "github" ],
  "size": 40,
  "stars": 5,
  "watches": 40,
  "discussion": 100500,
  "quality": 99.9,
  "due": "2013-08-18T16:00:55.581Z",
  "discusses": 10,
  "tags": [
    "bug",
    "ui",
    "@high"]
}  

````

* owner, project, number -- describes issue address (<owner>/<project>/issues/<number>)
* languages
* frameworks
* size -- project size (Kb)
* stars
* watches
* discussion -- discussion length
* quality
* due
* discusses -- how many people discusses issue
* tags