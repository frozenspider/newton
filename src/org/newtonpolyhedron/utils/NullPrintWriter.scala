package org.newtonpolyhedron.utils

import java.io.PrintWriter
import java.io.OutputStream

object NullPrintWriter extends PrintWriter(new OutputStream { override def write(b: Int) = {} })