/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import GenJarLicense._

additionalJarLicenseText := Some(
"""
  This product bundles source from 'Passera', including all files under the
  following directories:
    - passera/
    - passera/
  These files are available under the BSD-2-Clause license:

    Copyright (c) 2011-2013, Nate Nystrom
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright notice, this
    list of conditions and the following disclaimer in the documentation and/or
    other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
    FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
    OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  This product bundles source from 'Scala', including the following files:
    - org/apache/daffodil/util/UniquenessCache.class
  These files are available under the BSD-3-Clause licnese:

    Copyright (c) 2002-  EPFL
    Copyright (c) 2011-  Lightbend, Inc.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

    * Neither the name of the EPFL nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS”
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

  This product bundles material copied or derived from W3C, including the
  following files:
    - org/apache/daffodil/xsd/XMLSchema.dtd (https://www.w3.org/2001/XMLSchema.dtd)
    - org/apache/daffodil/xsd/XMLSchema.xsd (https://www.w3.org/2001/XMLSchema.xsd)
    - org/apache/daffodil/xsd/xml.xsd (https://www.w3.org/2001/xml.xsd)
    - org/apache/daffodil/xsd/datatypes.dtd (https://www.w3.org/2001/datatypes.dtd)
    - org/apache/daffodil/xsd/XMLSchema_for_DFDL.xsd (https://www.w3.org/2001/XMLSchema.dtd)
  These files are available under the W3C Software and Document Licnese:

    By obtaining and/or copying this work, you (the licensee) agree that you have
    read, understood, and will comply with the following terms and conditions.

    Permission to copy, modify, and distribute this work, with or without
    modification, for any purpose and without fee or royalty is hereby granted,
    provided that you include the following on ALL copies of the work or portions
    thereof, including modifications:

    * The full text of this NOTICE in a location viewable to users of the
      redistributed or derivative work.

    * Any pre-existing intellectual property disclaimers, notices, or terms and
      conditions. If none exist, the W3C Software and Document Short Notice should be
      included.

    * Notice of any changes or modifications, through a copyright statement on the
      new code or document such as "This software or document includes material
      copied from or derived from [title and URI of the W3C document]. Copyright ©
      [YEAR] W3C® (MIT, ERCIM, Keio, Beihang)."

    Disclaimers

    THIS WORK IS PROVIDED "AS IS," AND COPYRIGHT HOLDERS MAKE NO
    REPRESENTATIONS OR WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT
    LIMITED TO, WARRANTIES OF MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
    PURPOSE OR THAT THE USE OF THE SOFTWARE OR DOCUMENT WILL NOT INFRINGE ANY
    THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR OTHER RIGHTS.

    COPYRIGHT HOLDERS WILL NOT BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL OR
    CONSEQUENTIAL DAMAGES ARISING OUT OF ANY USE OF THE SOFTWARE OR DOCUMENT.

    The name and trademarks of copyright holders may NOT be used in advertising
    or publicity pertaining to the work without specific, written prior
    permission. Title to copyright in this work will at all times remain with
    copyright holders.
""")
