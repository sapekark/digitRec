# Digit Recogniser

Digit reconiser is a web application, which allows the user to input a handwritten digit to for a neural network to classify.

#Instructions 
(NOTE: The following instructions are designed and guaranteed for Windows computers, although with a little adapting, you should be able to follow them on other operating systems, too.)

Using the program is simple. 
First, make sure that you have the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) and [git](https://git-scm.com/) installed on your computer.

Now open you're command prompt, and traverse to a folder, where you wish to install the program. After this, type the following commands, in order, to command prompt:

1. git clone https://github.com/sapekark/digitRec.git

2. cd digitRec

3. stack build

4. cd src 

5. stack ghci Main.hs

The program is now starting. To initialize the web page for the application, type the following command: 

* main 

The page has now been initialized. Now, open your browser and type the following into the address bar.

* localhost:3000/

Further instructions on how to start using the program are presented on the web page.
