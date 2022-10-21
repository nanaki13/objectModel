import com.google.javascript.jscomp.*
import com.google.javascript.jscomp
import scala.collection.JavaConverters.*
import java.io.File
import java.nio.file.Paths
import java.nio.charset.Charset
import java.io.FileWriter
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.io.Source
object JsOpti  {

  object CharsetDef{
      implicit val default : Charset =Charset.forName("utf-8")
  }

  def compileTo(f: File,out : File)(implicit ch: Charset ):Unit = {
        val fw = new FileWriter(out,ch)
        Try(fw.append(compile(f))) match {
          case Success(value) => 
          case Failure(e) => throw e 
        }
  }

  def compile(f: File)(implicit ch: Charset ): String = {
    val compiler: jscomp.Compiler = new Compiler();
    val options: jscomp.CompilerOptions = new CompilerOptions();

    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(
      options
    );
 
   // Source.fromFile(f,"utf-8").getLines().map{}
    val nSource = sbt.io.IO.read(f,ch).replace("\\uff3f","_")

    val nSourceFile = f.toPath().getParent().resolve("clean.js")
    val fw = new FileWriter(nSourceFile.toFile(),ch)
    println("copy source")
    Try{
      fw.append(nSource) 
      fw.flush()
      fw.close()

    }match {
         case Success(_) => 
          val input: SourceFile =
            SourceFile.fromPath(nSourceFile,ch)
        
          compiler.compile(List[SourceFile]().asJava,(input :: Nil).asJava, options);
          compiler.toSource();
        case Failure(e) => throw e 
    }
   
  }

 

}
object Test extends App{
   import JsOpti.CharsetDef.default
    Paths.get("""C:\Users\Jonathan\Desktop\objectModel\game-engine\core\js\target\scala-3.1.2\game-engine-fastopt.js.map""").toFile().delete()
    println(JsOpti.compileTo(Paths.get("""C:\Users\Jonathan\Desktop\objectModel\game-engine\core\js\target\scala-3.1.2\game-engine-fastopt.js""").toFile(),
      new File("main.js")
    ))
}
