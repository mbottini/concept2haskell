# CS 557 Final Project: ErgParse

## Mike Bottini

# Introduction

Concept2 is a company that manufactures *ergometers* - literally devices
that measure work. Their flagship product is a rowing machine, but they
also manufacture a ski machine and an exercise bike.

The company has an online logbook to post workouts, and their online community
uses this logbook to count towards various challenges that are posted on their
website. The workouts consist of how many meters you rowed, (or skiied, or
biked) the duration of the workout, and other information regarding splits,
heart rate, and so on.

After working out on the machine, the data from that workout can be saved onto
a USB drive (or a proprietary smartcard, for the older models), and a
proprietary Windows application parses the data and sends the workouts to the
online logbook.

Unfortunately, this program only works on Windows. Furthermore, it doesn't work
on WINE, and this is a serious pain for me. I have to boot up an instance of
Windows in VirtualBox just to upload a few bytes of data, and this is very
silly. Lastly, the data itself is opaque; it's a compact binary format.

**I want to create a program that will duplicate some of the functionality of
the proprietary Concept2 Windows Utility.**

## Aside

I'm not quite sure how to format this, but as the A Perfect Circle song goes,
"Just begin." So, I'm going to format this exactly the way that the rubric
shows. It's ugly as far as a report goes, but I just need to stop staring at
the screen thinking "*Twenty pages???*"

# Objective 4

> "Describe what has been done."

## How a Rowing Machine works

A Concept2 rowing machine looks like this:

![rowing machine](images/concept2-model-d.jpg)

(Image pulled from https://www.concept2.com/indoor-rowers/model-d)

There's a bar, attached to a heavy flywheel by a bicycle chain. During the
*drive*, you straighten your legs and pull the bar the bar toward your chest.
This spins the flywheel. The seat slides back with you as you pull.
After you've fully extended, you *recover* by bending your legs and letting
the bar come away from your chest. The seat slides forward toward the flywheel.

The flywheel has fins on it, so air resistance will slow the flywheel during
the recovery. The machine measures the flywheel's acceleration and translates
that to some electronic analogue of moving a boat the equivalent distance across
the water. The slowing of the flywheel corresponds to the boat slowing down
as it glides across the water.

Rowing runs the gamut for duration and intensity. It's possible to row a
marathon, and it's also possible to do high-intensity training by sprinting
repeatedly for 500 meters. The standard competition distance in crew is 2,000
meters, which occupies an unhappy medium.

## Types of Workout

The rowing machine has a variety of workouts for you to choose from,
including the following:

* "Fixed Distance" - you row $X$ meters, and the machine counts down from $X$.
It will automatically divide the workout into **splits**, similar to laps on
a track. The splits measure how long it took you to row $Y$ meters, and by
default $Y$ is a fifth of the workout. So, if you row 5,000 meters, the
rowing machine will by default save how long it took you to row each 1k
split. We'll get into splits later.

* "Fixed Time" - you row $T_1$ minutes and $T_2$ seconds, and the machine
counts down from $T_1+T_2$. Same as the Fixed Distance workout, the machine
divides the workout into splits, except the splits are now in seconds, and
it measures how far you rowed during the split.

* "Just Row" - the rowing machine doesn't set any countdown; it just tracks how
far you row and how long you row for. It still keeps track of splits in the
"Fixed Time" format.

* "Distance Intervals" - you row $X$ meters, rest $T$ seconds, row $X$
meters... The machine tracks how long it took you to do the interval in a similar
manner to the splits, and it also tracks how much you row during the rest
time (although if you're rowing during your rest period, you probably didn't
work very hard). Note that it'll happily let you row as many intervals as you
want. You don't have to set $n$ intervals or anything.

* "Time Intervals" - you row $T_1$ seconds, rest $T_2$ seconds, row $T_1$
seconds... Similar to the Distance Intervals, the machine tracks each interval
and the distance that you row during the rest time.

* "Fixed Calorie / Calorie Intervals" - same as the above four workouts, but
with calories instead of meters or time. Unfortunately for me, the rowing
machine converts this internally to meters, and this is not a trivial
conversion.

* "Variable Intervals" - you can mix and match any of the interval types
(meters, seconds, and calories). And for each interval, you can also set a
separate rest time. Unlike the above intervals, you set the parameters for
$n$ intervals and the machine will end the workout after you row all of them.

In general, the workout will track the following:

* When did you do the workout? (time and date)

* How far did you row?

* How long did it take you to do it?

* What was your stroke rate, measured in strokes per minute?

* If you hooked up a heart rate monitor, what was your average heart rate?

* (For intervals) How long did you rest for, and how far did you row while
resting?

There are other parameters, but they aren't really relevant to what I'm
worried about. Honestly, the above proved to be more than enough stuff to
work with.

On modern rowing machines, (the PM5 model) the machine will save workouts
onto a USB drive, and that's what we're after.

## The Data Format

Let's say that you've rowed some workouts. You have a USB drive, and the
rowing machine has saved your workout(s) into a folder on the drive.
Obscurely, the folder is named `Concept2`. Inside the folder, we have the
following:

    mike@homebox:~/Desktop$ tree Concept2/
    Concept2/
    |-- DiagLog
    |   |-- DiagLog-430072297-20190117-123247.txt
    |   |-- DiagLog-430228525-20180220-210845.txt
    |   `-- DiagLog-430684965-20190220-123517.txt
    |-- Firmware
    |-- Getting\ Started.html
    |-- Logbook
    |   |-- DeviceLogInfo.bin
    |   |-- Favorites.bin
    |   |-- LogDataAccessTbl.bin
    |   |-- LogDataStorage.bin
    |   |-- LogStrokeInfo.bin
    |   |-- StrokeDataAccessTbl.bin
    |   |-- StrokeDataStorage.bin
    |   |-- UserDynamic.bin
    |   `-- UserStatic.bin
    `-- Special

The `DiagLog` isn't helpful here - it contains a record of firmware updates.
The meat is in `Logbook`... and more specifically, it's in two files:
`LogDataAccessTbl.bin` and `LogDataStorage.bin`.

Alexander Weinhold at https://gutmet.org provided an unofficial specification of
the format, which is mostly correct but incomplete and required some additional
reverse engineering.

### LogDataAccessTbl.bin

Each entry in `LogDataAccessTbl.bin` is 32 bytes and corresponds to a
workout. There are only three important members of the entry:
the workout type, the record offset, and the size of the record. The format is
as follows:

#### TODO: Add cute picture of LogDataAccessTbl format

The record offset corresponds to the workout's actual location in
`LogDataStorage.bin`, An offset of `0x100` would mean that the
workout starts at the `0x100`th byte of `LogDataStorage.bin`.

The workout type is important because inside `LogDataStorage.bin`, different
types of workouts are in different formats. For example, Fixed Distance workouts
must be parsed differently than Variable Interval workouts. The record size
is important because it gives us an easy way to grab only the bytes of the
file that correspond to this workout. We don't have to parse the data itself
to figure out how large the entry is.

Because we're dealing with pretty low-level hardware, the last 32 bytes of the
file are all `0xFF`. This is a way of saying "Hey, we're at the end of the file,"
I guess. This just means that we have to lop off the very last entry when we
parse the data.

### LogDataStorage.bin

`LogDataStorage.bin` is much more open-ended because the workouts could be
of different sizes. For example, I could have a Variable Interval workout
with 10 intervals, and I could also have a Fixed Distance workout where I
set the splits to be half of the distance I was rowing.

Each entry contains a **header**. "Split" workouts - Just Row, Fixed Distance,
Fixed Time, and Fixed Calorie - have a header size of 50 bytes.
Interval workouts have a header size of 52 bytes. The header formats are as follows:

#### TODO: Add cute diagram of header

After the header comes $n$ **frames**, which contain information about the $n$
splits or intervals in the workout. "Split" workouts and "Fixed Interval"
workouts have a frame size of 32 bytes. Variable Interval workouts have a
frame size of 48 bytes.

The frame formats are as follows:

#### TODO: Add cute diagrams of frames

---

Alright, I think we've stated enough of the problem to get started. Let's
dive in.

## File IO With Bytestreams

Typically, we deal with IO last in Haskell. The idea is that we define our
pure functions first, and then we take a deep breath, carve a couple of
protective runes into our desks, and dabble just enough in side effects to
get the job done without summoning Zalgo to the Realm of Men.

Unfortunately, in this case, I really didn't want to come up with a bunch of
binary numbers to test my functions. I needed the actual data first. This meant
that I needed to deal with IO first and foremost.

From work in class, we know that any data that gets read from a file is
encapsulated in the `IO` monad. The exact same applies with binary files,
except we want bytes instead of `Char`s. The Haskell wiki page
https://wiki.haskell.org/Dealing_with_binary_data has a pretty sparse
description of the `Get` monad, which we need to turn a `ByteString` into a
list of `Word8`s.

There's probably a better more Haskell-ish way to do it, but I wrote the
following function. From `ergparse.hs`:

    gBytes :: Get [Word8]
    gBytes = do
    e <- isEmpty
    case e of
        True -> return []
        False -> do
        current <- getWord8
        rest <- gBytes
        return (current : rest)

    -- Opens a binary file and returns an IO containing the bytes in the list.
    getLogDataAccessData :: [Char] -> IO [Word8]
    getLogDataAccessData filename = do
        bStream <- B.readFile filename
        return $ runGet gBytes bStream

The first function returns a `Get` of `[Word8]`. The second function takes a
filename and returns an `IO` containing the list of `Word8`s in the list. So,
I can now do

    *Main> getLogDataAccessData "LogDataAccessTbl.bin"
    [240,3,0,0,74,52,...]

From the above Data Format section, I'm looking for a magic `0xF0` value in the 0th
byte, and that's 240. Looks good to me.

The exact same thing applies with the `LogDataStorage.bin` file. I'm looking for a
magic `0x95` value to start a header. So, I should get a `149` as my first byte.

    *Main> getLogDataAccessData "LogDataStorage.bin"
    [149,3,0,1,25,164...]

How 'bout that.

## Working With Lists

Going back to Week 2 of the class, we did some work with lists. Well, now we've got a
big list of `Word8`s. I define a few more list operations that I couldn't find in the
Prelude or `Data.List`. They're probably somewhere in the standard libraries, though.

One thing that I really want is *slicing*. For one, I need to grab slices of
the list to turn a big list of bytes into a smaller list of bytes that
contain just the data of the current workout. And then within that smaller
list, I have still smaller lists of multi-byte entries. For example, the
timestamp is a 4-byte entry, the total distance of the workout is a 4-byte
entry, and the split size is a 2-byte entry.

From `Utils.hs`, which you're going to see a lot of.

    grabChunk :: Int -> Int -> [a] -> [a]
    grabChunk 0 0 _  = []
    grabChunk _ _ [] = []
    grabChunk 0 amount (x:xs) = x : grabChunk 0 (amount-1) xs
    grabChunk offset amount (x:xs) = grabChunk (offset-1) amount xs

This function is analoguous to the Python "slice" operation. I want `amount`
elements from `offset` indices into the list, as a list.

Getting the first header in `LogDataStorage.bin` is a trival example of this.
Note the return of our friend `inIO`, which Mark used to great effect in the
file IO section of our class.

    *Main> :{
    *Main| getLogDataAccessData "LogDataStorage.bin" >>= 
    *Main|     (Utils.inIO (Utils.grabChunk 0 50))
    *Main| :}
    [149,3,0,1,25,164,196...] (50 bytes)

And within those 50 bytes, let's grab the total distance, which is contained
in bytes 24-27, inclusive.

    *Main> :{
    *Main| getLogDataAccessData "LogDataStorage.bin" >>=
    *Main|     Utils.inIO (Utils.grabChunk 0 50) >>=
    *Main|     Utils.inIO (Utils.grabChunk 24 4)
    *Main| :}
    [0,0,19,136]

Finally, we can do the exact same thing with bytes as Mark did with bits in class.
We're going to add up these bytes to get a single integer.

Again, from `Utils.hs`:

    parseLittleEndian :: [Word8] -> Int
    parseLittleEndian [] = 0
    parseLittleEndian (x:xs) = (fromIntegral x) + 256 * (parseLittleEndian xs)

    parseBigEndian :: [Word8] -> Int
    parseBigEndian = parseLittleEndian . reverse

    *Main> :{
    *Main| getLogDataAccessData "LogDataStorage.bin" >>=
    *Main|     Utils.inIO (Utils.grabChunk 0 50) >>=
    *Main|     Utils.inIO (Utils.grabChunk 24 4) >>=
    *Main|     Utils.inIO Utils.parseBigEndian
    *Main| :}
    5000

I rowed a 5k, so that's a good sign.

### Oh, we need bits, not just bytes?!

There's one special case here - the datestamp is in four bytes, but the
year, month, day, hour, and minute are arranged strangely for compactness.
The year is the first seven bits, the day is 5 bits, the month is 4 bits,
the hour is 8 bits, and the minute is 8 bits.

Well, let's use exactly what we covered in class (and coincidentally, I ran
into this problem exactly when Mark started covering it).

Again, from `Utils.hs`:

type Bit = Bool

    wordToBits :: Word8 -> [Bit]
    wordToBits = wordToBits' 0

    wordToBits' :: Int -> Word8 -> [Bit]
    wordToBits' 8 w = []
    wordToBits' n w
        | odd w = True : wordToBits' (n+1) (div w 2)
        | otherwise = False : wordToBits' (n+1) (div w 2)

    wordsToBits :: [Word8] -> [Bit]
    wordsToBits xs = xs >>= wordToBits

    wordsToBitsReverse :: [Word8] -> [Bit]
    wordsToBitsReverse xs = xs >>= (reverse . wordToBits)

    bitsToInt :: [Bit] -> Int
    bitsToInt [] = 0
    bitsToInt (True:xs) = 1 + 2 * (bitsToInt xs)
    bitsToInt (False:xs) = 2 * (bitsToInt xs)


Note that `Word8` is an instance of `Integral`, so we can happily
`div` it just like any other integer. The only part that we didn't
cover in class was that the `>>=` operator for the list monad is
just `concatMap`, so we can get a big list of `Bit`s from a list of
`Word`s by binding with the `wordToBits` function.

And now we can `grabChunk` this list of bits to get the individual
date fields! Let's get the year of our first workout. As stored, it's
the number of years since 2000. The timestamp is bytes 8 to 11 of the
header.

Unfortunately, due to endianness issues, we have to do a whole bunch
of reversing - once for the order of the bytes, and once for the
order of the bits themselves. Gross.

    parseYear :: [Bit] -> Int
    parseYear bs = 2000 + (bitsToInt . reverse $ bs)

In `ghci`:

    *Main> :{
    *Main| getLogDataAccessData "LogDataStorage.bin" >>=
    *Main|     Utils.inIO (Utils.grabChunk 0 50) >>=
    *Main|     Utils.inIO (Utils.grabChunk 8 4) >>=
    *Main|     Utils.inIO Utils.wordsToBitsReverse >>=
    *Main|     Utils.inIO (Utils.grabChunk 0 7) >>=
    *Main|     Utils.inIO Utils.parseYear
    *Main| :}
    2017

This is my very first workout with this rowing machine, and yes, I did it in
2017.


## Creating a JSON object

So what's the actual point of this mess? Well, the Concept2 Logbook API
demands a JSON object of a particular format. A simplified format is as follows:

    {
        "type" : "rower",
        "workout_type" : "FixedDistanceSplits",
        "date": "2017-02-21 16:20:00",
        "distance" : 5000,
        "time" : 13386.0, // tenths of a second!
        "workouts" : {
            "splits" : [
                // individual objects for each split
            ]
        }
    }

In some cases, there is a 1:1 correspondence. For example, the JSON object
must include the date, and there is a datestamp in the header. However,
others are not so simple. For example, each split must contain the distance
and time that were rowed. However, the split size is actually contained in
the header!

So, it's not enough just to say "Well, the frame has fields $x$, $y$, and
$z$, so just create an object associating these fields to strings and call it
a day." We have to merge objects, or the JSON object will be incomplete.

So, after parsing, we have to do a little bit of massaging to take data
from the header and put it into the frames. In others, we have to take
data from the frames and put it into the object. For example, the
VariableDistance workout does not have an average stroke rate for the workout.
Instead, we have to calculate it from the stroke rates of each interval.

## Endgame

I wasn't able to get to this because Concept2's response time is very slow, but
the original goal was to actually send the requests as POST requests to their
API. Unfortunately, they need an OAuth2 key, and it takes months to get it from
them. I actually started the project pretty early, and they just got back to me
with some background questions on my program a couple weeks ago. I'll
probably get some sort of response from them sometime in May. So, I've sadly
resorted to using their online validator tool at 
https://log.concept2.com/developers/validator. This is a tool from Concept2 to
check to see if a JSON object meets their requirements.



# Objective 7

> "Cite your sources."


# Objective 1

> "Uses, applies, and extends techniques and ideas that we have covered in
class."

## Lists and List Processing

The most important aspect of this program is parsing the binary format. Just
as a text parser consumes a list of tokens, mine uses bytes. This means that
pretty much all of my `parseDataType` functions are of the type

    parseDataType :: [Word8] -> DataType

I have a variety of functions in the `Utils.hs` file, which my various parser
functions call to break up these lists and transform them into the data
structures.

Because the 