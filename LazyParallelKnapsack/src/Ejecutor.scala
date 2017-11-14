import monix.eval.Task;
import monix.execution.Scheduler.Implicits.global;
import scala.concurrent.Await
import scala.concurrent.duration._

object Ejecutor extends App {
//no se usa
//para no tener que poner la palabra implicit delante de cada metodo de TareaMonada
//object objTareaMonada {
//  implicit def instance: TareaMonada =  new TareaMonada
//}
  val Capacidad = 10000;
  val Valores = List(0,9,13,153,50,15,68,27,39,23,52,11,32,24,48,73,42,43,22,7,18,4,30);
  val Pesos = List(0,150,35,200,160,60,45,60,40,30,10,70,30,15,10,40,70,75,80,20,12,50,10);
  
  //funcion resuelve problema de la mochila
  def m(numeroElementos:Int, capacidad:Int):Int ={
    if(numeroElementos<0){
      return 0;
    }else if(capacidad==0){
      return 0;
    }else if(Pesos(numeroElementos)>capacidad){
      return m(numeroElementos-1,capacidad);
    }else{
      val sinoMete = m(numeroElementos-1,capacidad);
      val siMete = m(numeroElementos-1,capacidad-Pesos(numeroElementos))+Valores(numeroElementos);
      return math.max(sinoMete, siMete);
     }
  }  

  //println(m(Pesos.length-1,Capacidad));
  
  //---------------------------------------
  //----------------EJECUCION--------------
  //tarea1 : Task[Int]
  //terminadas(0)=false;terminadas(1)=false;terminadas(2)=false;
  val tarea1 = Task{m(Pesos.length-1,Capacidad)};
  val tarea2 = Task{m(Pesos.length-1,400)};
  val tarea3 = Task{m(Pesos.length-1,200)};
  //secuenciaTareas : Seq[Task[Int]]
  val secuenciaTareas = Seq(tarea1, tarea2, tarea3);
  //tareaSecuenciaAgregadas : Task[Seq[Int]]
  val tareaSecuenciaAgregadas = Task.gatherUnordered(secuenciaTareas);//.map(_.toList);
  val tini = System.nanoTime();
  val future = tareaSecuenciaAgregadas.runAsync;
  Await.result(future, 3.seconds);
  val tfin = System.nanoTime();
  future.foreach(println); 
  val nanoTiempo = (tfin-tini);
  println("tiempo de ejecucion: "+(nanoTiempo/1000000)+"ms");
  Thread.sleep(2000);
}