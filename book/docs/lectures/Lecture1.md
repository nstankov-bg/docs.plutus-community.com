# Lecture #1 - Week01 Plutus Pioneer Program

## Credits
Condensed version of Lecture #1 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://youtu.be/IEn6jUo-0vU)
Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/mqdh90/week_01_contract_englishauction_plutus_pioneer/)

## Setup

1 - First clone the Git Repository of the Plutus Pioneer Program:

    [$] git clone https://github.com/input-output-hk/plutus-pioneer-program
    [$] cd plutus-pioneer-program/code/week01

2 - Build the week01 project

    [$] cabal build

**Note:** this might take a while (go grab some coffee and a cookie)

Open \*2\* new terminal tabs/windows

CD into your Plutus installation directory

**Note:** for installation instructions: check the other articles in the docs


3 - In the first terminal

Start the plutus playground server

    [$] cd plutus-playground-server
    [$] plutus-playground-server




And in the second terminal, start the client

    [$] cd plutus-playground-client
    [$] npm start




Once they are both up and running, and the build of week01 is done:



Go to your browser of choice to: [https://localhost:8009](https://localhost:8009)



The playground should pop up here.



Now for the Example Contract from week01:



4 - Remove all the code currently in code editor

Copy all the code in the `plutus-pioneer-program/code/week01/src/Week01/EnglishAuction.hs` file

And paste it into (the now empty) code editor

(should be around 368 lines of code)



For it to compile though, we need to remove some lines of code.

At the top of the file, a Haskell module header is located.

But since the Playground adds it's own wrapper, this is obsulate and should be removed.



5 - Remove these lines:

    module Week01.EnglishAuction
        ( Auction (..)
        , StartParams (..), BidParams (..), CloseParams (..)
        , AuctionSchema
        , start, bid, close
        , endpoints
        , schemas
        , ensureKnownCurrencies
        , printJson
        , printSchemas
        , registeredKnownCurrencies
        , stage
        ) where




You may now press the nice green "compile" button top right of your screen.

Now we are ready to play with the contract.

## Simulation

Press "simulate" (blue button in the top right).



This opens the Simulate window, where we can try out our newly compiled contract.

This defaults to 2 wallets, to make things interesting though, add another one. (the big "add wallet" button)



The whole idea of this contract is to auction off an NFT (Non-Fungible Token).

Each wallet has 10 lovelaces, and 10 T (T is the Token here).

Change the total of T's for Wallet1 to 1, and for Wallet2 and 3, to 0. (if there were more than 1 it wouldn't be an NFT ofcourse)

_**Simply put:**_ Wallet1 is going to put up for auction 1T, and Wallet2-3 will be bidding.



As you can see, each wallet has the functions "bid", "close" and "start".

**Bid** -> places a bid of x lovelaces

**Start** -> starts the bidding procedure with getSlot (how long will the bidding last for), spMinBid (minimal lovelaces required)

**Close** -> closes the bidding; gives the highest bidder its NFT/Token



**Note:** "pay to wallet" is always there, don't worry about that now :)



[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/g4w8mnj5r0t61.png?width=968&format=png&auto=webp&s=fa29ff20500aa56e2774e6d0a0d84284c9b3a5d4)](https://preview.redd.it/g4w8mnj5r0t61.png?width=968&format=png&auto=webp&s=fa29ff20500aa56e2774e6d0a0d84284c9b3a5d4)



6 - Wallet1 is going to put the Token up for auction, we'll do this by pressing the "start" button at Wallet1.

This will add an Action to the Action Sequence.



Here we need to set the parameters:

**getSlot:** 20 (the bidding will close on slot 20)

**spMinBid:** 3 (atleast 3 lovelaces are required)

**spCurrency:** 66 (the currencysymbol for the T token; will be explained in future lectures)

**spToken:** T (the Token)



[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/qag2ffs7r0t61.png?width=316&format=png&auto=webp&s=6b75cf9e5fd4e886880897d22fa73939dc84ea3c)](https://preview.redd.it/qag2ffs7r0t61.png?width=316&format=png&auto=webp&s=6b75cf9e5fd4e886880897d22fa73939dc84ea3c)

Wallet 1



7 - Next we need to add a wait action (1 slot).

This will give all the actions time to be executed.



[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/k3ofbhi9r0t61.png?width=320&format=png&auto=webp&s=a1ba4fac171b17c503def39da8a37ed24026ef45)](https://preview.redd.it/k3ofbhi9r0t61.png?width=320&format=png&auto=webp&s=a1ba4fac171b17c503def39da8a37ed24026ef45)



8 - Now for this example Wallet2 will start the bidding with a Bid of 3 lovelaces.

Press the "bid" button at Wallet2, and update the Action with the parameters:

**spCurrency:** 66 (Same as above)

**spToken:** T (the Token)

**bpBid:** 3 (how much lovelaces)



[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/j2xc9wiar0t61.png?width=321&format=png&auto=webp&s=684dbc484a9cf8b702fd04c1b458ba194eb26fe0)](https://preview.redd.it/j2xc9wiar0t61.png?width=321&format=png&auto=webp&s=684dbc484a9cf8b702fd04c1b458ba194eb26fe0)

Wallet 2



9 - Insert another wait action here (1 slot)



10 - Now Wallet3 also wants to place a bid.

Same as Wallet2, add a "bid" action, with all the same parameters as above; except for the bpBid parameter.

This could be set to anything (min. 3), but for this example we'll set it to 5.



[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/ekgbkrzbr0t61.png?width=315&format=png&auto=webp&s=f07a612bdad692299bdd9f7a0211b740888751cd)](https://preview.redd.it/ekgbkrzbr0t61.png?width=315&format=png&auto=webp&s=f07a612bdad692299bdd9f7a0211b740888751cd)

Wallet 3



Great. The whole bidding sequence is *DONE.*



11 - To finish the bidding, we'll add yet another wait action; only this time we'll "wait until" slot 20.

(remember the first action? At slot 20 the bidding will be closed!)

After this the last function (close) still needs to be added, to finalize the bidding sequence.

We will call this from Wallet1, so add the "close" action from Wallet1, with the correct parameters (you know what to do).



[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/cl2z2ccdr0t61.png?width=317&format=png&auto=webp&s=aee55aa7390e92b029eb3cfda09a8d513b13a7f1)](https://preview.redd.it/cl2z2ccdr0t61.png?width=317&format=png&auto=webp&s=aee55aa7390e92b029eb3cfda09a8d513b13a7f1)



[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/ei15tit0s0t61.png?width=316&format=png&auto=webp&s=246838413dc21abd94a816d2b0a6b00a2fb644b2)](https://preview.redd.it/ei15tit0s0t61.png?width=316&format=png&auto=webp&s=246838413dc21abd94a816d2b0a6b00a2fb644b2)



12 - Last but not least, we'll add another wait action here (1 slot)


## Evaluation

**Great we're done with the whole setup!**



To execute everything on the simulated blockchain, press the green "Evaluate" button on the bottom of your screen.



On the next screen you'll see the individual slots.

[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/exzozcyfr0t61.png?width=800&format=png&auto=webp&s=7ada65e6443b2d594099374195c0bf50c33f78f7)](https://preview.redd.it/exzozcyfr0t61.png?width=800&format=png&auto=webp&s=7ada65e6443b2d594099374195c0bf50c33f78f7)



_**Slot 0, Tx 0**_ -> the Genesis slot. This is there to setup everything.

Wallet1 -> 1T and 10 lovelaces, Wallet2 -> 10 lovelaces, Wallet3 -> 10 lovelaces

_**Slot 1, Tx 0**_ -> The start action, this is where Wallet1 transfers it's 1T (the Token) to the Contract.

_**Slot 2, Tx 0**_ -> The bid of Wallet2 (3 lovelaces)

**Note:** The contract now has 1T and 3 lovelaces

_**Slot 3, Tx 0**_ -> The bid of Wallet3 (5 lovelaces)

**Note:** The contract now has 1T and 5 lovelaces; Wallet2 gets it's 3 lovelaces back

_**Slot 20, Tx 0**_ -> Here Wallet3 has won the bidding "war", and is granted its 1T! Also Wallet1 gets its 5 lovelaces :)

**Note:** The contract now does not have anything at all :) everything is nicely given to it's rightful owners.


13 - To check the final output of all wallets, scroll down to the "Final balances" section.

As you can see, Wallet3 now has the 1T.

[![r/cardano - Week 01 Contract - EnglishAuction - Plutus Pioneer Program](https://preview.redd.it/2yswtd1ir0t61.png?width=1188&format=png&auto=webp&s=25a895aa510ff3e7faa7f3fb25b8f91d4f7ffcf2)](https://preview.redd.it/2yswtd1ir0t61.png?width=1188&format=png&auto=webp&s=25a895aa510ff3e7faa7f3fb25b8f91d4f7ffcf2)


*Great!*

If you made it all the way down here; Awesome!

Although I would still recommend you watching [the original lecture](https://youtu.be/IEn6jUo-0vU) by Lars Brünjes on YouTube.

It contains some great nitty gritty tips&tricks.

Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/mqdh90/week_01_contract_englishauction_plutus_pioneer/)
