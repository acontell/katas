# Tetris Kata #

https://gist.github.com/jameskbride/f7c6cbf133294518f1995744aa90a8e4

- First part of this kata centers on developing the functionality of tetris. It was done in a
"outside in" style of TDD
- Second part of this kata (user interface) was done in a more "inside out" style of TDD.

The main idea was to completely decouple part 1 from part 2 in a way that
changing the visualization scheme was simple: right now it uses the browser but porting it to some
other device is easy and only affects code in the ui folder.

If you want to try it, you'll need to generate the javascript for index.html first. In order to do so, you need npm installed in your machine and run:

- npm run-script build

Once you've done it, just open index.html in your favourite browser and enjoy.