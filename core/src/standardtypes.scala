package sbinary;

import Operations._;
import scala.collection._;
import generic.CanBuildFrom
import shapeless._

/**
  * Generates [[Format]]s any "product" (tuple or case class),
  * provided there are implicit [[Format]]s for every composing element.
  */
trait ProductTypes {

  implicit val formatHNil: Format[HNil] = new Format[HNil] {
    override def reads(in: Input): HNil = HNil
    override def writes(out: Output, value: HNil): Unit = ()
  }

  implicit def formatHList[Head, Tail <: HList](
                                                 implicit formatHead: Lazy[Format[Head]],
                                                 formatTail: Format[Tail]) =
    new Format[Head :: Tail] {
      override def reads(in: Input): Head :: Tail =
        formatHead.value.reads(in) :: formatTail.reads(in)
      override def writes(out: Output, value: Head :: Tail): Unit = {
        formatHead.value.writes(out, value.head)
        formatTail.writes(out, value.tail)
      }
    }

  implicit def formatProduct[A, Gen <: HList](
                                               implicit gen: Generic.Aux[A, Gen],
                                               formatHList: Format[Gen]): Format[A] =
    new Format[A] {
      override def reads(in: Input): A = gen.from(formatHList.reads(in))
      override def writes(out: Output, value: A): Unit =
        formatHList.writes(out, gen.to(value))
    }

}

trait BasicTypes extends CoreProtocol with ProductTypes {
  implicit def optionsAreFormat[S](implicit bin : Format[S]) : Format[Option[S]] = new Format[Option[S]]{
    def reads(in : Input) = read[Byte](in) match {
      case 1 => Some(read[S](in));
      case 0 => None
    }

    def writes(out : Output, s : Option[S]) = s match {
      case Some(x) => { write[Byte](out, 1); write(out, x) }
      case None => write[Byte](out, 0);
    }
  }

}

trait LowPriorityCollectionTypes extends Generic {
  def canBuildFormat[CC[X] <: Traversable[X], T](implicit bin : Format[T], cbf: CanBuildFrom[Nothing, T, CC[T]]) : Format[CC[T]] =
    new LengthEncoded[CC[T], T]{
      def build(length : Int, ts : Iterator[T]) = {
        val builder = cbf.apply()
        builder.sizeHint(length)
        builder ++= ts
		  if(ts.hasNext) sys.error("Builder did not consume all input.") // no lazy builders allowed
        builder.result()
      }
    }
}

trait CollectionTypes extends BasicTypes with LowPriorityCollectionTypes {
  implicit def listFormat[T](implicit bin : Format[T]) : Format[List[T]] = canBuildFormat[List, T]

  implicit def arrayFormat[T](implicit fmt : Format[T], mf : scala.reflect.Manifest[T]) : Format[Array[T]] = fmt match{
    case ByteFormat => ByteArrayFormat.asInstanceOf[Format[Array[T]]];
    case _ =>
      new CollectionFormat[Array[T], T]{
        def build(length : Int, ts : Iterator[T]) = {
          val result = new Array[T](length);
          ts.copyToArray(result, 0);
          result;
        }
        def size(a: Array[T]) = a.length
        def foreach(a: Array[T])(f: T => Unit) = a foreach f
      }
    }

  implicit object ByteArrayFormat extends Format[Array[Byte]]{
    def reads(in : Input) = {
      val length = read[Int](in);
      val bytes = new Array[Byte](length);
      in.readFully(bytes);
      bytes;
    }

    def writes(out : Output, bytes : Array[Byte]): Unit = {
      write(out, bytes.length);
      out.writeAll(bytes);
    }
  }

  implicit def mutableSetFormat[T](implicit bin : Format[T]) : Format[mutable.Set[T]] =
    viaSeq((x : Seq[T]) => mutable.Set(x :_*))

  implicit def immutableSetFormat[T](implicit bin : Format[T]) : Format[immutable.Set[T]] =
    viaSeq((x : Seq[T]) => immutable.Set(x :_*))

  implicit def immutableSortedSetFormat[S](implicit ord : Ordering[S], binS : Format[S]) : Format[immutable.SortedSet[S]] = {
    viaSeq( (x : Seq[S]) => immutable.TreeSet[S](x :_*))
  }

  implicit def immutableMapFormat[S, T](implicit binS : Format[S], binT : Format[T]) : Format[immutable.Map[S, T]] =
    viaSeq( (x : Seq[(S, T)]) => immutable.Map(x :_*));

  implicit def immutableSortedMapFormat[S, T](implicit ord : Ordering[S], binS : Format[S], binT : Format[T]) : Format[immutable.SortedMap[S, T]] = {
    viaSeq( (x : Seq[(S, T)]) => immutable.TreeMap[S, T](x :_*))
  }

  /**
   * Format instance for streams.
   * Note that unlike almost all other collections this is not length encoded
   * Instead it is encoded with a sequence of byte separators, with a single
   * byte value of 1 preceding each element to be read and a value of 0 indicating
   * the stream termination.
   *
   * This is to ensure proper laziness behaviour - values will be written as they
   * become available rather than thunking the entire stream up front.
   *
   * Warning! The resulting Stream is not read lazily. If you wish to read a Stream
   * lazily you may consider it to be a sequence of Option[T]s terminated by a None.
   *
   * Note that this behaviour has changed from that of SFormat 0.2.1, though the format
   * remains the same.
   */
  implicit def streamFormat[S](implicit bin : Format[S]) : Format[Stream[S]] = new Format[Stream[S]]{
    def reads(in : Input) = {
      val buffer = new mutable.ArrayBuffer[S];
      while((read[Option[S]](in) match {
        case Some(s) => buffer += s; true;
        case None => false;
      })){};
      buffer.toStream;
    }

    def writes(out : Output, stream : Stream[S]): Unit = {
      stream.foreach(x => { write[Byte](out, 1); write(out, x); });
      write[Byte](out, 0);
    }
  }
}

trait StandardTypes extends CollectionTypes{
  implicit object BigIntFormat extends Format[BigInt]{
    def reads(in : Input) = BigInt(read[Array[Byte]](in));
    def writes(out : Output, i : BigInt) = write(out, i.toByteArray);
  }

  implicit object BigDecimalFormat extends Format[BigDecimal]{
    def reads(in : Input) = BigDecimal(read[String](in));
    def writes(out : Output, d : BigDecimal) = write(out, d.toString);
  }

  implicit object ClassFormat extends Format[Class[_]]{
    def reads(in : Input) = Class.forName(read[String](in));
    def writes(out : Output, clazz : Class[_]) = write(out, clazz.getName);
  }

  implicit lazy val SymbolFormat : Format[Symbol] = viaString(Symbol(_));

  import java.io.File;
  implicit lazy val FileFormat : Format[File] = viaString(new File(_ : String));

  import java.net.{URI, URL}
  implicit lazy val UrlFormat : Format[URL] = viaString(new URL(_ : String));
  implicit lazy val UriFormat : Format[URI] = viaString(new URI(_ : String));


  import scala.xml.{XML, NodeSeq};
  implicit lazy val XmlFormat : Format[NodeSeq] = new Format[NodeSeq]{
    def reads(in : Input) = XML.loadString(read[String](in)).child;
    def writes(out : Output, elem : NodeSeq) = write(out, <binary>elem</binary>.toString);
  }
}
