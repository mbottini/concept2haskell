# PM5 Data Format

Created by Alexander Weinhold. His site is http://gutmet.org.


The data is split across two files, LogDataAccessTbl.bin and LogDataStorage.bin. The former provides information about the location of the actual workout data in LogDataStorage.

## LogDataAccessTbl.bin

Each entry in **LogDataAccessTbl** is 32 bytes, multibyte entries are Little Endian:

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

( * unimportant, because either redundant in actual record or unreliable )


### Workout Types

| Value | Type              |
|-------|-------------------|
| 0x01  | Free Row          |
| 0x03  | Single Distance   |
| 0x05  | Single Time       |
| 0x06  | Timed Interval    |
| 0x07  | Distance Interval |
| 0x08  | Variable Interval |
| 0x0A  | Single Calorie    |
| 0x0C  | Calorie Interval  |


## LogDataStorage.bin

Each entry in **LogDataStorage** has a header and a number of splits or intervals. The header size is either 50 bytes (workout types 0x01, 0x03, 0x05, 0x0A) or 52 bytes (others). The size of each split frame is 32 bytes for all workouts except type 0x08, where it is 48 bytes. Multibyte entries are, contrary to LogDataAccessTbl.bin Big Endian(!)

### Header (types 0x01 - 0x05, 0x0A)

| Byte  | Meaning            |
|------:|--------------------|
| 0     | Magic 0x95         |
| 1     | Type of workout    |
| 2-3   | N/A                |
| 4-7   | Serial number      |
| 8-11  | Timestamp          |
| 12-13 | User ID            |
| 14-17 | N/A                |
| 18    | Record ID          |
| 19-21 | Magic 0x000000     |
| 22-23 | Total Duration     |
| 24-27 | Total Distance     |
| 28    | SPM                |
| 29    | Split Info         |
| 30-31 | Split Size         |
| 32-49 | N/A                |

### Header (Fixed Intervals)

| Byte  | Meaning             |
|------:|---------------------|
| 0-18  | As above            |
| 19    | Number of splits    |
| 20-21 | Split size          |
| 22-23 | Interval rest time  |
| 24-27 | Total Work Duration |
| 28-29 | Total Rest Distance |
| 30-51 | N/A                 |

### Header (Variable Intervals)

| Byte  | Meaning             |
|------:|---------------------|
| 0-18  | As above            |
| 19    | Number of splits    |
| 20-23 | Total Work Duration |
| 24-27 | Total Work Distance |
| 30-51 | N/A                 |



### Timestamp format (bytes 8-11)

| Bits  | Meaning          |
|------:|------------------|
| 0-6   | year after 2000  |
| 7-11  | day              |
| 12-15 | month            |
| 16-23 | hour             |
| 24-31 | minute           |


### Split Frame (Non-Interval Types)

| Byte  | Meaning                 |
|------:|-------------------------|
| 0-1   | Split Duration/Distance |
| 2     | Heart Rate              |
| 3     | SPM                     |
| 4-31  | N/A                     |

### Interval Frame (Timed Interval, Distance Interval)

| Byte  | Meaning                 |
|------:|-------------------------|
| 0-1   | Split Duration/Distance |
| 2     | Heart Rate              |
| 3     | Rest Heart Rate         |
| 4     | SPM                     |
| 5-31  | N/A                     |

### Variable Interval Frame

| Byte  | Meaning                 |
|------:|-------------------------|
| 0     | Split Type              |
| 1     | SPM                     |
| 2-5   | Work Interval Time      |
| 6-9   | Work Interval Distance  |
| 10    | Heart Rate              |
| 11    | Rest Heart Rate         |
| 12-13 | Interval Rest Time      |
| 14-15 | Interval Rest Distance  |
| 16-47 | N/A                     |
