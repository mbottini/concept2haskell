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

# ErgParse Implementation

> "Describe what has been done."
- Funlangs Project Grading Rubric

## How a Rowing Machine works

A Concept2 rowing machine looks like this:

![rowing machine](images/concept2-model-d.jpg)
\

(Image pulled from https://www.concept2.com/indoor-rowers/model-d)

There's a bar, attached to a heavy flywheel by a bicycle chain. During the
*drive*, you straighten your legs and pull the bar toward your chest.
This spins the flywheel. The seat slides back with you as you pull. After
you've fully extended, you *recover* by bending your legs and letting the bar
come away from your chest. The seat slides forward toward the flywheel.

The flywheel has fins on it, so air resistance will slow the flywheel during
the recovery. The machine measures the flywheel's acceleration and translates
that to some electronic analogue of moving a boat the equivalent distance across
the water. The slowing of the flywheel corresponds to the boat slowing down
as it glides across the water.

Rowing runs the gamut for duration and intensity. It's possible to row a
marathon, and it's also possible to do high-intensity training by sprinting
repeatedly for 100-500 meter intervals. The standard competition distance in
crew is 2,000 meters, which occupies an unhappy medium.

## Types of Workout

The rowing machine has a variety of workouts for you to choose from,
including the following:

* "Fixed Distance" - you row $X$ meters, and the machine counts down from $X$
as you do it. It will automatically divide the workout into **splits**,
similar to laps on a track. The splits measure how long it took you to row
$Y$ meters, and by default $Y$ is a fifth of the workout. So, if you row
5,000 meters, the rowing machine will by default save how long it took you to
row each 1k split. We'll get into splits later.

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
machine converts this internally to meters, and it is not a trivial
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
as follows, directly quoted from Alexander Weinhold:

| Byte  | Meaning                             |
|------:|-------------------------------------|
| 0     | Magic 0xF0                          |
| 1     | Workout type                        |
| 2-3   | Interval rest time*                 |
| 4-5   | Workout name*                       |
| 6-7   | N/A                                 |
| 8-9   | Timestamp*                          |
| 10-11 | N/A                                 |
| 12-13 | No. of Splits*                      |
| 14-15 | Duration/Distance*                  |
| 16-17 | Record offset in LogDataStorage.bin |
| 18-23 | N/A                                 |
| 24-25 | Size of record in bytes             |
| 26-27 | Index                               |
| 28-31 | N/A                                 |

(* unimportant, because either redundant in actual record or unreliable)

(Back to my analysis)

Note that there are only three genuinely important aspects to each entry:

* The workout type, which determines how big the header and frames will be.
The format of those is shown below.

* The record offset, which determines how many elements of the `LogDataStorage`
list I have to `drop`. An offset of `0x100` would mean that the
workout starts at the `0x100`th byte of `LogDataStorage.bin`.

* The size of the record, which determines how many of the rest that I have to
`take`.

Because we're dealing with pretty low-level hardware, the last 32 bytes of
the file are all `0xFF`. This is a way of saying "Hey, we're at the end of
the file," I guess. For the purposes of this program, it just means that we
have to lop off the very last entry when we parse the data. I wrote an
`allButLast` function to do this for me.

### LogDataStorage.bin

`LogDataStorage.bin` is much more open-ended because the workouts could be
of different sizes. For example, I could have a Variable Interval workout
with 10 intervals, and I could also have a Fixed Distance workout where I
set the splits to be half of the distance I was rowing.

Each entry contains a **header**. "Split" workouts - Just Row, Fixed
Distance, Fixed Time, and Fixed Calorie - have a header size of 50 bytes.
Interval workouts have a header size of 52 bytes. All tables are quoted
almost directly from Alexander Weinhold but with added types from me and with
a couple of minor corrections.

| Byte  | Meaning            | Type               |
|------:|--------------------|--------------------|
| 0     | Magic 0x95         | (N/A)              |
| 1     | Type of workout    | Enum               |
| 2-3   | N/A                |                    |
| 4-7   | Serial number      | Int                |
| 8-11  | Timestamp          | Special: See below |
| 12-13 | User ID            | Int                |
| 14-17 | N/A                |                    |
| 18    | Record ID          | Int                |
| 19-21 | Magic 0x000000     | (N/A)              |
| 22-23 | Total Duration     | Int                |
| 24-27 | Total Distance     | Int                |
| 28    | Strokes per Minute | Int                |
| 29    | Split Info         | (???) (Not sure)   |
| 30-31 | Split Size         | Depends on type    |
| 32-49 | N/A                |                    |

The timestamp is uniquely compact. I'm not sure why, they had some extra
space reserved from Bytes 32-49. It's of the following format:

| Bits  | Meaning          |
|------:|------------------|
| 0-6   | year after 2000  |
| 7-11  | day              |
| 12-15 | month            |
| 16-23 | hour             |
| 24-31 | minute           |

The only genuinely confusing aspect of this is the "Split Info" value. I
couldn't figure out what this was. It didn't seem to be relevant, but it's
here.

### Header (Fixed Intervals)

| Byte  | Meaning             | Type               |
|------:|---------------------|--------------------|
| 0-18  | As above            |                    |
| 19    | Number of splits    | Int                |
| 20-21 | Split size          | Depends on type    |
| 22-23 | Interval rest time  | Seconds            |
| 24-27 | Total Work Duration | Tenths of a second |
| 28-29 | Total Rest Distance | Int                |
| 30-51 | N/A                 |                    |

### Header (Variable Intervals)

| Byte  | Meaning             | Type               |
|------:|---------------------|--------------------|
| 0-18  | As above            |                    |
| 19    | Number of splits    | Int                |
| 20-23 | Total Work Duration | Tenths of a second |
| 24-27 | Total Work Distance | Int                |
| 30-51 | N/A                 |                    |

Note the wonderful inconsistency between seconds and tenths of a second. This
wasn't described in Alexander Weinhold's specification, but it wasn't too
hard to do basic sanity checks and figure out "Hey, I rested for 2 minutes,
not 20."

After the header comes $n$ **frames**, which contain information about the $n$
splits or intervals in the workout. "Split" workouts and "Fixed Interval"
workouts have a frame size of 32 bytes. Variable Interval workouts have a
frame size of 48 bytes.

The frame formats are as follows:

### Split Frame (Non-Interval Types)

| Byte  | Meaning                 | Type            |
|------:|-------------------------|-----------------|
| 0-1   | Split Duration/Distance | Depends on Type |
| 2     | Heart Rate              | Int             |
| 3     | Strokes per Minute      | Int             |
| 4-31  | N/A                     |                 |

### Interval Frame (Timed Interval, Distance Interval)

| Byte  | Meaning                 | Type            |
|------:|-------------------------|-----------------|
| 0-1   | Split Duration/Distance | Depends on Type |
| 2     | Heart Rate              | Int             |
| 3     | Rest Heart Rate         | Int             |
| 4     | Strokes Per Minute      | Int             |
| 5-31  | N/A                     |                 |

### Variable Interval Frame

| Byte  | Meaning                 | Type               |
|------:|-------------------------|--------------------|
| 0     | Split Type              | Enum (always 0??)  |
| 1     | Strokes Per Minute      | Int                |
| 2-5   | Work Interval Time      | Tenths of a second |
| 6-9   | Work Interval Distance  | Int                |
| 10    | Heart Rate              | Int                |
| 11    | Rest Heart Rate         | Int                |
| 12-13 | Interval Rest Time      | Seconds            |
| 14-15 | Interval Rest Distance  | Int                |
| 16-47 | N/A                     |                    |

---

Alright, I think we've stated enough of the problem to get started. Let's
dive in.

## File IO With Bytestreams

Typically, we deal with IO last in Haskell. The idea is that we define our
pure functions first, and then we take a deep breath, carve a couple of
protective runes into our desks, and dabble just enough in side effects to
get the job done without summoning Zalgo to the Material Plane.

Unfortunately, in this case, I really didn't want to come up with a bunch of
binary numbers to test my functions. I needed the actual data first. This meant
that I needed to deal with IO first and foremost.

From work in class, we know that any data that gets read from a file is
encapsulated in the `IO` monad. The exact same applies with binary files,
except we want bytes instead of `Char`s. The Haskell wiki page
https://wiki.haskell.org/Dealing_with_binary_data has a pretty sparse
description of the `Get` monad, which we need to turn a `ByteString` into a
list of `Word8`s.

There's probably a better and more Haskell-ish way to do it, but I wrote the
following function. From `Utils.hs`:

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
    grabChunk offset amount = take amount . drop offset

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

I like this method of programming because we can see each transformation
as it occurs. We grab the bytes from a file, grab the header-sized chunk,
grab the timestamp-sized chunk, convert to bits, grab the year-sized chunk
of bits, and then parse it into our year value.

All of my parsing functions work this way, albeit with composition rather
than the `>>=` operator. I only use the `IO` monad for the operation of
getting the list of bytes from the file. There are a variety of these
functions for converting `Word8` lists to various types, and we'll use
them to build our own product types containing workout data.

One other really common type here is the `DiffTime`, defined in `Data.Time`.
I use this to measure durations of splits, intervals, total workout time,
and so on. Some of the entries in the data format expect seconds, and others
demand tenths of a second. For whatever reason, `DiffTime` only takes two
inputs: seconds and picoseconds. So, my "tenth of a second" function looked
like this:

    parseDuration :: [Word8] -> DiffTime
    parseDuration = picosecondsToDiffTime . 
                    (*100000000000) .
                    toInteger . 
                    parseBigEndian

That's a lot of zeros.

## Type Insanity

Since we've got a ton of fields in the various workouts, we're going to
want to create product types. We didn't cover it in class, but record
syntax proved to be extremely helpful here, both for creating the type
and creating a parsing function. Here's a `DistanceIntervalHeader`
type as an example.

    import qualified DataTypes.WorkoutType as Wt
    import qualified Utils
    -- other imported types like Data.Time
    
    data DistanceIntervalHeader = DistanceIntervalHeader {
        workoutType :: Wt.WorkoutType,
        serialNumber :: Int,
        timeStamp :: LocalTime,
        userID :: Int,
        recordID :: Int,
        numSplits :: Int,
        splitSize :: Int,
        restTime :: DiffTime,
        totalTime :: DiffTime,
        totalRestDistance :: Int
    } deriving(Show)
    
    parseDistanceIntervalHeader :: [Word8] -> DistanceIntervalHeader
    parseDistanceIntervalHeader ws = DistanceIntervalHeader {
        workoutType = Wt.parseWorkoutType . fromIntegral . (!! 1) $ ws,
        serialNumber = Utils.parseBigEndian . Utils.grabChunk 4 4 $ ws,
        timeStamp = Utils.parseDateStamp . Utils.grabChunk 8 4 $ ws,
        userID = Utils.parseBigEndian . Utils.grabChunk 12 2 $ ws,
        recordID = fromIntegral . (!! 18) $ ws,
        numSplits = fromIntegral . (!! 19) $ ws,
        splitSize = Utils.parseBigEndian . Utils.grabChunk 20 2 $ ws,
        restTime = Utils.parseSecs . Utils.grabChunk 22 2 $ ws,
        totalTime = Utils.parseDuration . Utils.grabChunk 24 4 $ ws,
        totalRestDistance = Utils.parseBigEndian . Utils.grabChunk 28 2 $ ws
    }

In short - each type has its definition in record syntax, and a parsing
function that takes a `[Word8]` and composes the various `Utils` functions
to parse `chunk`s of the list.

This is ugly, but it's not *that* hard to figure out. Doing this for every one
of the various types was tedious, though. There were some ways to abstract some
of it and create some subordinate product types, but the hierarchy itself started
to get confusing, and I found that it was easier to keep track of what was
going on with a flatter but more verbose type hierarchy.

By doing it this way, I have three "tedious" sets of types, plus the
`TableEntry` type.

* Frames - two `Split` frames, and four `Interval` frames.

* Headers - one for each type of workout.

* Workouts - one for each type of workout.

There's also a `Workout` sum type that encapsulates every specific `Workout`.
This allows me to start with a big list of bytes and end up with a `[Workout]`
list at the end.

I found that **namespaces** were invaluable. Haskell doesn't work the same way
with object methods as object-oriented languages do. If I have two `Header`
types and have a `restTime` function to access a field in the types, they
conflict; there is no overloading allowed. To get around this, I put every type
in its own separate file and `import qualified`. Now, if I have
`DistanceIntervalHeader` and `TimeIntervalHeader`, I can call either one with
`Dih.totalRestTime` and `Tih.totalRestTime`, respectively.

Since we looked at a `DistanceIntervalHeader`, let's look at the
`DistanceIntervalFrame`. It's a lot simpler, as it holds much less data.

    data DistanceIntervalFrame = DistanceIntervalFrame {
        duration :: DiffTime,
        heartRate :: Int,
        restHeartRate :: Int,
        strokesPerMinute :: Int
    } deriving(Show)
    
    parseDistanceIntervalFrame :: [Word8] -> DistanceIntervalFrame
    parseDistanceIntervalFrame ws = DistanceIntervalFrame {
        duration = Utils.parseDuration . Utils.grabChunk 0 2 $ ws,
        heartRate = fromIntegral . (!! 2) $ ws,
        restHeartRate = fromIntegral . (!! 3) $ ws,
        strokesPerMinute = fromIntegral . (!! 4) $ ws
    }

And lastly, let's look at our overarching `DistanceIntervalWorkout`. Note
the qualified import abbreviations.

    data DistanceIntervalWorkout = DistanceIntervalWorkout {
        tableEntry :: Te.TableEntry,
        header :: Dih.DistanceIntervalHeader,
        frames :: [Dif.DistanceIntervalFrame]
    } deriving(Show)
    
Just like we have `Utils` functions to parse little chunks of binary data,
we have the larger `parseTableEntry`, `parseDistanceIntervalHeader`, and
`parseDistanceIntervalFrame` functions to do the same with larger chunks.

Because the binary data is split between two files, I found that it was
easier to get all of the `TableEntry` values first, and then use those to
get the `Header` and `Frame`s for each `Workout`. Each `TableEntry` contains
the record offset and the record size for its header and frames, so I could
create a function that takes a `TableEntry` and a list of bytes and returns
a `Workout`. Each `Workout` contains this `getFrames` function. From
the `DistanceIntervalWorkout`, and representative of the rest:

    getFrames :: Te.TableEntry -> [Word8] -> DistanceIntervalWorkout
    getFrames te ds = DistanceIntervalWorkout {
        tableEntry = te,
        header = Dih.parseDistanceIntervalHeader chunk,
        frames = map Dif.parseDistanceIntervalFrame . 
                 Utils.splitAll Consts.frameSize .
                 drop Consts.intervalHeaderSize .
                 Utils.grabChunk offset index $ ds
    }
        where chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
              offset = Te.recordOffset te
              index = Te.recordSize te

We get the `offset` and `index` from the `TableEntry`, get a `chunk` of bytes
from the list, get the `header` from the first `intervalHeaderSize` bytes of
the `chunk`, and then `splitAll` the remainder of the `chunk` into
`frameSize`-sized` "subchunks" and get the `frames` from mapping our parse
function over each of those subchunks.

Since we declared all of these types to derive `Show`, we can start printing
them. An early iteration of my program, before I took the next step, looked
like this:

    getTableEntries :: [Char] -> IO [Te.TableEntry]
    getTableEntries filename = Utils.getLogDataAccessBinaries filename >>=
                            Utils.inIO Utils.allButLast >>=
                            Utils.inIO (map Te.parseTableEntry) 

    getWorkouts :: [Te.TableEntry] -> [Word8] -> [W.Workout]
    getWorkouts tes ds = map (\te -> W.getFrames te ds) tes

    main = do
        entries <- getTableEntries "LogDataAccessTbl.bin"
        workoutData <- Utils.getLogDataAccessData "LogDataStorage.bin"
        mapM_ (putStrLn . show) (getWorkouts entries workoutData)

So we parse the `TableEntry` values from `LogDataAccessTbl.bin`, making
sure to discard the last one. Then we get the `WorkoutData`, a big list
of bytes, from `LogDataStorage.bin`. We can now get the entire
list of workouts with `getWorkouts`, which calls the `getFrames` function
I defined above.

Printing this is, um, ugly, but it's definitely a start.

![Raw Data before transformation](images/nojson.png)
\

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

Enter `Data.Aeson`, an external library that has a bunch of functionality
for parsing JSON to Haskell types and serializing Haskell types to JSON.
I'm using very little of its functionality, as I'm only serializing. So,
the extent of my use of this library consists of making each of my types
into `instance`s of `ToJSON`.

In some cases, there is a 1:1 correspondence. For example, the JSON object
must include the date, and there is a datestamp in the header. However,
others are not so simple. For example, each split must contain the distance
and time that were rowed. However, the split size is actually contained in
the header!

Here's an example of this from `DistanceIntervalFrame`:

    instance ToJSON DistanceIntervalFrame where
        toJSON dif = object [
            "type" .= String "distance",
            "time" .= (Utils.tenthsToScientific . duration $ dif),
            "stroke_rate" .= (Utils.intToScientific . strokesPerMinute $ dif)]

We have the time here, but we don't have the distance. So, it's not enough
just to say "Well, the frame has fields $x$, $y$, and $z$, so just create an
object associating these fields to strings and call it a day." We have to
merge objects, or the JSON object will be incomplete.

In some cases, we have to take data from the header and put it into the frame
serialization. In others, we have to take data from the frames and put it
into the object. For example, the VariableDistance workout does not have an
average stroke rate for the workout. Instead, we have to calculate it from
the stroke rates of each interval.

My general approach is to build as much of an object as we can at the `Frame`
and `Header` level, and then at the `Workout` level, where we have access to
both `Object`s, we merge them and add more fields with attributes from both
`Object`s.

I defined the following utility functions in `Utils` to do this.

    mergeObjects :: Value -> Value -> Value
    mergeObjects (Object x) (Object y) = Object $ HML.union x y
    mergeObjects _ _ = error "Can't merge non-object values!"

    addAttribute :: String -> Value -> Value -> Value
    addAttribute s val (Object x) = Object $ HML.insert (DT.pack s) val x
    addAttribute _ _ _ = error "Can only add attribute to object!"

Note that since we're dealing with `Value`s, we lose a lot of the type-checking
that made other Haskell programming so easy. We're now in the realm of Python
and runtime errors. Joy.

To take a couple of small examples, we can do the following:

    -- Utils imports Data.Aeson, but we need Data.Aeson.Encode.Pretty and
    -- Data.ByteString.Lazy.Char8 because encode returns a ByteString, not a
    -- String. We also need the OverloadedStrings language extension.
    Prelude> :l Utils
    [1 of 1] Compiling Utils            ( Utils.hs, interpreted )
    Ok, one module loaded.
    *Utils> import Data.Aeson.Encode.Pretty as P
    *Utils P> import qualified Data.ByteString.Lazy.Char8 as BC
    *Utils P BC> :set -XOverloadedStrings
    *Utils P BC> let o = object ["foo" .= "bar", "baz" .= 5]
    *Utils P BC> let p = object ["quux" .= True, "spam" .= 7]
    *Utils P BC> putStrLn . BC.unpack . encodePretty $ mergeObjects o p
    {
        "foo": "bar",
        "baz": 5,
        "spam": 7,
        "quux": true
    }

Similarly, for `addAttribute`:

    *Utils P BC> :{
    *Utils P BC| putStrLn . BC.unpack . encodePretty $
    *Utils P BC|     addAttribute "eggs" (Number 5) p
    *Utils P BC| :}
    {
        "spam": 7,
        "eggs": 5,
        "quux": true
    }

Because functions are curried, we can compose `addAttribute` functions
to add multiple key-value pairs in a (hopefully) readable manner.

    *Utils P BC> :{
    *Utils P BC| putStrLn . BC.unpack . encodePretty $
    *Utils P BC|     addAttribute "eggs" (Number 5) .
    *Utils P BC|     addAttribute "jam" "strawberry" $ p
    *Utils P BC| :}
    {
    "jam": "strawberry",
    "spam": 7,
    "eggs": 5,
    "quux": true
    }
    

Here's an example from our friend the `DistanceIntervalWorkout`. It's ugly,
and there was probably a better way to do this.

    -- Merge object made of "stroke_rate", "distance", and compound "workout"
    -- object to the toJSON transformation of the header. Note that the frames
    -- themselves must also have attributes added to them.
    instance ToJSON DistanceIntervalWorkout where
        toJSON w = Utils.mergeObjects derivedValues (toJSON (header w))
            where derivedValues = (object ["workout" .= object ["intervals" .= fs],
                                        "stroke_rate" .= sr,
                                        "distance" .= dt])
                numIntervals = Dih.numSplits . header $ w
                sr = Number (Utils.intToScientific .
                            Utils.average . 
                            map Dif.strokesPerMinute .
                            frames $ w)
                dt = Number (Utils.intToScientific .
                            (* numIntervals) . 
                            Dih.splitSize .
                            header $ w)
                fs = populateFrames (header w) (frames w)

    -- Add "rest_time" and "distance" key-value pairs to each frame.
    populateFrames :: Dih.DistanceIntervalHeader -> 
                    [Dif.DistanceIntervalFrame] ->
                    Value
    populateFrames dih = 
        listValue id .
        map (Utils.addAttribute "rest_time" rt) .
        map (Utils.addAttribute "distance" dt) .
        map toJSON
            where rt = Number $ Utils.tenthsToScientific . Dih.restTime $ dih
                dt = Number $ Utils.intToScientific . Dih.splitSize $ dih

We have the JSON values in the `header`, and we have the ability to map
`toJSON` to the list of `frames` and turn them into a single `Array` with
`listvalue id`. But the rest of the values have to be derived, so there are a
bunch of nasty computations that are then merged into the object. I dearly
missed Scala's ability to have some state and do `map += (foo -> bar)`, as it
was a lot more readable than the above mess.

Other `toJSON` implementations are similarly ugly due to this need to access
values from both objects, do calculations on them, and then add the results
to the objects themselves. Any individual step isn't too hard, but it piles
up really quickly and becomes unmanageable. I don't like it, and my guess is
that my approach was fundamentally flawed.

## Endgame

I wasn't able to get to this because Concept2's response time is very slow, but
the original goal was to actually send the requests as POST requests to their
API. Unfortunately, they need an OAuth2 key, and it takes months to get it from
them. I actually started the project pretty early, and they just got back to me
with some background questions on my program a couple weeks ago. I'll
probably get some sort of response from them sometime in May. So, I've sadly
resorted to using their online validator tool at 
https://log.concept2.com/developers/validator. This is a tool from Concept2 to
check to see if a JSON object meets their requirements. I can copy-paste
the object into their field, and it will print its interpretation of the object
and whether it's a valid object. Here's an example:

![Screenshot from Validator Page](images/validator.png)
\

This has a couple of nice aspects to it.

* It does some minimal validation to ensure that all of the required attributes
are in the object.

* It prints a readable format of what it interprets my workout to entail. This
means that if I, for example, provided a crazy rest time, didn't track
interval distances properly, or something similar, it would show non-sensical
data in the bottom left corner. When testing the final result, I would put in
a bunch of different types of workouts and ensure that all of them got sane
results from this validator.

# Testing

Ummm... Well, this is part of the rubric, isn't it? Unfortunately, I'm going
to have to take the L here, as I mostly coded by the seat of my pants. I wasn't
even sure what the format of most of the data was in, so the real way to see
if the code worked was if it spat out workouts that looked vaguely like the
ones that I did.

The `Utils` were really the only code that lent itself to unit tests. I made
a couple of functions to do this, but they were pretty half-hearted. The most
difficult and test-heavy part of my program was the `parseDateStamp`
function, which involved a large number of my other list processing
functions and a whole bunch of trial and error to figure out the endianness.

Besides that, it mostly came down to copy-pasting the finished JSON data into
the Validator and looking at what it returned. I had a whole bunch of very
minor problems with things like the following:

* Rest time wasn't added properly. 

* Times that should have been in tenths of a second were in seconds, and vice
versa.

* Attributes were missing from various steps of the object, and I had to put
them in.

Because Concept2 provided this for me, I didn't do very much tooling for
myself.

# Stuff I learned

Timezones are mean, nasty, ugly things. The Concept2 API wanted me to merge
its object with a timezone from the `tz` database, and apparently this is
incredibly difficult to do on Linux. It seems to be happy with my `"PST"`
`TimeZone` that I got from using `Data.Time.LocalTime`, and I tested it with
a couple of other random timezones from the Indian Ocean and whatnot. This
isn't quite accurate, but I couldn't figure out a non-awful way to get
`"America/Los_Angeles"` instead of `PST`.

Point-free programming was an absolute joy. Being able to chain functions
together with composition was outright beautiful in certain places, and
was at least legible in others. I may have gone overboard in some areas.

The hard-and-fast namespace/module requirement was actually really beneficial
for breaking up the program and forcing me to think about where I should put
some of the utility functions. They didn't all end up in `Utils`; some of
them ended up with their corresponding types. Also, breaking up the program
into multiple files encouraged me to be even more aggressive about keeping
complexity inside only one file at a time and making a friendly interface for
the next higher level.

There was no reason for me to name my parsing functions
`parseDistanceIntervalHeader` and whatnot. I could've named them `parse`, and
what's more is that I learned this lesson halfway through; all of my
`getFrames` functions are named the same thing and kept distinct from one
another with the above namespace requirement. Not a big deal, but a little
extra typing that didn't need to happen.

If I had to rewrite the program, I would have done more work with `class`es.
I had a couple of really repetitive sections of code where I was writing

    foo :: Supertype -> bar
    foo (subtype1 w) = foo w
    foo (subtype2 w) = foo w

It's not *that* much more code, but as soon as I see stuff like that, I
know that the code could use some refactoring.

Up until I hit the JSON segment, which involves a lot of converting various
types to `String` and doesn't allow this, I had the luxury of being able to
say, "If it compiles, it's probably very, very close to correct." The type
system is absolutely amazing at making sure that you provide the correct
functions in the correct order to transform the data.

The IO monad is actually a lot less scary than it was when I started the
term. It's not quite a warm fuzzy thing yet, but it's getting there. I'd
advise FP newbies to stop reading the "a monad is just a monoid in the
category of endofunctors" garbage on the Internet and **use** it.

Everyone on the \#haskell IRC knows Mark Jones and thinks he's the cat's
pajamas.


# Works Cited

I am deeply indebted to the following people, in no particular order, for
their following contributions:

* Alexander Weinhold at https://gutmet.org, for providing the lion's share of
the PM5 data format. This project would have been an order of magnitude more
difficult without him. Link at
https://gutmet.org/blog/2018/03/04/Concept2-PM5-logbook-data-format.html

* Anu Dudhia, from the University of Oxford, for providing some functions
regarding the ergometer's conversion of calories to watts. Link at
http://eodg.atm.ox.ac.uk/user/dudhia/rowing/physics/ergometer.html#section11

* Concept2 for providing a function to convert watts to velocity. This
was useful when converting calories to meters. Link at
https://www.concept2.com/indoor-rowers/training/calculators/watts-calculator

* "Chuck," from Stack Overflow, for thoughts on list slicing. Link at
https://stackoverflow.com/a/4597877

* Artyom Kazak for his Data.Aeson tutorial. I didn't use much of it, but
what I did use, he made pretty clear. Link at https://artyom.me/aeson


