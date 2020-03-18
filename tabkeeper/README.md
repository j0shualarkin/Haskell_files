# TabKeeper

You have a file with a number of urls. 
Tabkeeper will help you pick a URL from that list,
remove it from the source file and save it in another file.

The source file is currently ```input.txt``` and the 
other file it is saved to is ```seen.txt```. 

We will want operations to rollback times we've asked 
for a url. That is, move an entry (the latest) from 
```seen.txt``` back to ```input.txt```. 

We want the choice of URL to be random but after 
showing the different kinds. This means displaying
to the user the different domains of the links.
Such as, "youtube.com" or "stackoverflow.com", and 
including a collection of "other" (for the random sites).



