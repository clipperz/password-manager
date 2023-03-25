package is.clipperz.backend.functions

import zio.Task
import zio.stream.ZStream
import is.clipperz.backend.functions.crypto.HashFunction

object ByteArrays:
  def hashOfArrays(hash: HashFunction, arrays: Array[Byte]*): Task[Array[Byte]] =
    hash(ZStream.fromIterable(arrays.foldLeft(Array[Byte]())(_ ++ _)))

  def bitxor(x: Array[Byte], y: Array[Byte]): Array[Byte] =
    x.zip(y).map(_ ^ _).map(i => i.toByte)

  def printArray(text: String, a: Array[Byte]): Unit =
    println(text + ": ")
    print("[ ")
    a.foreach(b => print(b.toString + " "))
    print(" ]")
    println()
