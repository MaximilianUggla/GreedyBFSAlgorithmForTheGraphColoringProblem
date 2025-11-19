import java.io.PrintWriter
import java.io.File
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.math.max

object Main extends App {

    case class Node(id: Int, neighbors: Array[Int], var color: Int = -1)

    def generateDirected() =
        val pw1 = new PrintWriter(new File("graphDir1.txt"))
        // val pw2 = new PrintWriter(new java.io.File("graphDir2.txt"))
        // val pw3 = new PrintWriter(new java.io.File("graphDir3.txt"))
        // val pw4 = new PrintWriter(new java.io.File("graphDir4.txt"))
        // val pw5 = new PrintWriter(new java.io.File("graphDir4.txt"))

        for node <- (0 until 10) do
            var str = node.toString()
            val nbrfOfEdges = Random.between(0, 9)
            val list = (Random.shuffle(0 until 10) diff IndexedSeq(node)).take(nbrfOfEdges)
            for n <- list do str = str + " " + n
            pw1.write(str + "\n")
        pw1.close()
        
        // for x <- (0 until 100) do
        //     var str = " "
        //     for y <- (0 until (Random.between(0, 100))) do str = str + (Random.between(0, 100)).toString + " "
        //     pw2.write(x.toString() + str + "\n")

        // for x <- (0 until 1000) do
        //     var str = " "
        //     for y <- (0 until (Random.between(0, 1000))) do str = str + (Random.between(0, 1000)).toString + " "
        //     pw3.write(x.toString() + str + "\n")

        // for x <- (0 until 10000) do
        //     var str = " "
        //     for y <- (0 until (Random.between(0, 10000))) do str = str + (Random.between(0, 10000)).toString + " "
        //     pw4.write(x.toString() + str + "\n")

        // for x <- (0 until 100000) do
        //     var str = " "
        //     for y <- (0 until (Random.between(0, 100000))) do str = str + (Random.between(0, 100000)).toString + " "
        //     pw5.write(x.toString() + str + "\n")
        
    def generateSymetric() =
        val pw1 = new PrintWriter(new java.io.File("graphSym1.txt"))
        val pw2 = new PrintWriter(new java.io.File("graphSym2.txt"))
        val pw3 = new PrintWriter(new java.io.File("graphSym3.txt"))
        val pw4 = new PrintWriter(new java.io.File("graphSym4.txt"))

    def setUp(file: String, nbrOfColors: Int) =
        val source = scala.io.Source.fromFile(file)
        val inputBuffer: ArrayBuffer[String] = ArrayBuffer().appendAll(source.getLines())

        val graph: Array[Node] = inputBuffer.map(str => 
            val IntArr = str.split(" ").map(_.toInt)
            Node(IntArr(0), IntArr.tail)
            ).toArray

        val blockedColors: Array[Set[Int]] = Array.fill(graph.size)(scala.collection.mutable.Set[Int]())

        val coloredGraph = colorGraph(graph, blockedColors, nbrOfColors, graph(0))

        verify(coloredGraph)

    def verify(graph: Array[Node]) =
        var verified = true
        for node <- graph do
            for neighbor <- node.neighbors do
                if node.color == graph.apply(neighbor).color then verified = false
        verified

    def colorGraph(graph: Array[Node], blockedColors: Array[Set[Int]], colorLimit: Int, startNode: Node): Array[Node] = 
        var colorCounter = 0
        val nodeQueue: Queue[Node] = Queue(startNode)
        val visitedNodes: Set[Int] = scala.collection.mutable.Set(startNode.id)
        var doRun = true

        while (!nodeQueue.isEmpty && doRun) do
            val current = nodeQueue.dequeue()
            var chosenColor = 0

            while (blockedColors(current.id).contains(chosenColor)) do
                chosenColor += 1

            if chosenColor <= colorLimit then 
                current.color = chosenColor
                colorCounter = max(colorCounter, chosenColor)
                for neighbor <- current.neighbors do
                    blockedColors(neighbor).add(chosenColor)
                    if !visitedNodes.contains(neighbor) then
                        visitedNodes.add(neighbor)
                        nodeQueue.append(graph(neighbor))
        println((colorCounter+1).toString() + " Colors used")
        graph

    generateDirected()

    // generateSymetric()

    println(setUp("graphDir1.txt", 10))
    // println(setUp("graphDir2.txt", 100))
    // println(setUp("graphDir3.txt", 1000))
    // println(setUp("graphDir4.txt", 10000))
    // println(setUp("graphDir5.txt", 100000))


    // println(setUp("graph1Sym.txt", 10))
    // println(setUp("graph2Sym.txt", 50))
    // println(setUp("graph3Sym.txt", 250))
    // println(setUp("graph4Sym.txt", 1250))
}