#!/usr/bin/python

###
 # @section LICENSE
 #
 # Licensed to the Apache Software Foundation (ASF) under one or more
 # contributor license agreements.  See the NOTICE file distributed with
 # this work for additional information regarding copyright ownership.
 # The ASF licenses this file to You under the Apache License, Version 2.0
 # (the "License"); you may not use this file except in compliance with
 # the License.  You may obtain a copy of the License at
 #
 #     http://www.apache.org/licenses/LICENSE-2.0
 #
 # Unless required by applicable law or agreed to in writing, software
 # distributed under the License is distributed on an "AS IS" BASIS,
 # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 # See the License for the specific language governing permissions and
 # limitations under the License.
###

import argparse
import hashlib
import struct

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--size", help="Specify the size of the generated file in MB", type=int)
    parser.add_argument("-o", "--output", help="Path to the file that will be generated")
    args = parser.parse_args()

    length_in_bytes = args.size * 1024000
    chunk  = bytearray.fromhex('deadbeef' * 1000)
    chunksize = len(chunk)
    bytes_written = 0
    hash_md5 = hashlib.md5()
    with open(args.output, 'wb') as fout:
        fout.write(struct.pack(">Q", length_in_bytes)) # 8-bytes big-endian
        while (bytes_written < length_in_bytes):
            fout.write(chunk)
            hash_md5.update(chunk)
            bytes_written += chunksize

    print("Wrote %d bytes" % bytes_written)
    print("Blob md5 hash: %s" % hash_md5.hexdigest())
