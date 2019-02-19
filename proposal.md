# CS557 Proposal: ErgParse

## Mike Bottini

## Introduction

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

## Goals

1. Create a data structure for `Workout`s and methods of parsing from the binary
blob files into the data structure.

2. Create a method for serializing `Workout`s into JSON objects, for sending
to the Concept2 online log.

3. Create a method to send this data to the logbook and detect whether the
logbook accepted the submission. This requires work with OAuth, as
Concept2's Logbook API requires authentication.

## Stretch Goals, Silly And Otherwise

4. (NOPE) The proprietary smartcard can be read with a USB card reader.
Unfortunately, no Linux drivers exist to read the card. But I can use
Wireshark to intercept the bytes that are sent by the Windows utility to my
card reader, and my guess is that it's rudimentary. I've also done a fair
amount of low-level interaction with USB devices at work.

  This turned out to be a Charlie Foxtrot. Not gonna happen.

5. Concept2 requires me to run my utility with the "development" version of
their logbook as a target, and then apply to them once it's ready to work on
production.  I don't know exactly what their application process entails; it
might involve rigorous testing, or it might be a check-in-the-box.

6. Create a GUI for the program instead of it running off of the command line,
ideally one that is similar to the official Concept2 utility. This will require
me to learn how Haskell plays with WxWidgets or other graphics libraries.

7. Expand the utility's functionality to include the SkiErg and maybe even the
BikeErg, if I can find one. There's a SkiErg at the PSU gym; I haven't come
across a BikeErg. There might be one at a local Crossfit affiliate whose owners
might let me mess with the machine for an hour.

## Current Progress

* A wonderful German guy named Alexander Weinhold
  ([http://gutmet.org](http://gutmet.org)) has posted an unofficial
specification of the binary format at
[http://git.gutmet.org/pm5conv/tree/dataformat](http://git.gutmet.org/pm5conv/tree/dataformat),
along with a simple Go program that parses it. I haven't looked at anything
that his Go program does; the only thing that I'm taking from him is his work
on the format.

* I'm about 90% through the parsing part of the program. I see this as the hard
part, but perhaps some of my other goals will run into a whole bunch of
difficulties.
