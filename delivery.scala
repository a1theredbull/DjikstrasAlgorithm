import collection.mutable.Map
import collection.mutable.HashMap

object Test {
	def main(args: Array[String]) = {
		var test1 = List(
			Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Mark's crib", "distance" -> 9),
			Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Greg's casa", "distance" -> 4),
			Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Matt's pad", "distance" -> 18),
			Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Brian's apartment", "distance" -> 8),
			Map("startLocation" -> "Brian's apartment", "endLocation" -> "Wesley's condo", "distance" -> 7),
			Map("startLocation" -> "Brian's apartment", "endLocation" -> "Cam's dwelling", "distance" -> 17),
			Map("startLocation" -> "Greg's casa", "endLocation" -> "Cam's dwelling", "distance" -> 13),
			Map("startLocation" -> "Greg's casa", "endLocation" -> "Mike's digs", "distance" -> 19),
			Map("startLocation" -> "Greg's casa", "endLocation" -> "Matt's pad", "distance" -> 14),
			Map("startLocation" -> "Wesley's condo", "endLocation" -> "Kirk's farm", "distance" -> 10),
			Map("startLocation" -> "Wesley's condo", "endLocation" -> "Nathan's flat", "distance" -> 11),
			Map("startLocation" -> "Wesley's condo", "endLocation" -> "Bryce's den", "distance" -> 6),
			Map("startLocation" -> "Matt's pad", "endLocation" -> "Mark's crib", "distance" -> 19),
			Map("startLocation" -> "Matt's pad", "endLocation" -> "Nathan's flat", "distance" -> 15),
			Map("startLocation" -> "Matt's pad", "endLocation" -> "Craig's haunt", "distance" -> 14),
			Map("startLocation" -> "Mark's crib", "endLocation" -> "Kirk's farm", "distance" -> 9),
			Map("startLocation" -> "Mark's crib", "endLocation" -> "Nathan's flat", "distance" -> 12),
			Map("startLocation" -> "Bryce's den", "endLocation" -> "Craig's haunt", "distance" -> 10),
			Map("startLocation" -> "Bryce's den", "endLocation" -> "Mike's digs", "distance" -> 9),
			Map("startLocation" -> "Mike's digs", "endLocation" -> "Cam's dwelling", "distance" -> 20),
			Map("startLocation" -> "Mike's digs", "endLocation" -> "Nathan's flat", "distance" -> 12),
			Map("startLocation" -> "Cam's dwelling", "endLocation" -> "Craig's haunt", "distance" -> 18),
			Map("startLocation" -> "Nathan's flat", "endLocation" -> "Kirk's farm", "distance" -> 3)
		)

		var test2 = List(
			Map("startLocation" -> "U", "endLocation" -> "V", "distance" -> 2),
			Map("startLocation" -> "U", "endLocation" -> "X", "distance" -> 1),
			Map("startLocation" -> "V", "endLocation" -> "X", "distance" -> 2),
			Map("startLocation" -> "V", "endLocation" -> "W", "distance" -> 2),
			Map("startLocation" -> "X", "endLocation" -> "W", "distance" -> 3),
			Map("startLocation" -> "X", "endLocation" -> "Y", "distance" -> 1),
			Map("startLocation" -> "W", "endLocation" -> "Y", "distance" -> 1),
			Map("startLocation" -> "W", "endLocation" -> "Z", "distance" -> 5),
			Map("startLocation" -> "Y", "endLocation" -> "Z", "distance" -> 2)
			)

		val test1Analyze = new Analyzer()
		test1Analyze.findDistances("Kruthika's abode", test1)
		println(test1Analyze.findShortestPath("Kruthika's abode", "Mark's crib"))
		println(test1Analyze.findShortestPath("Kruthika's abode", "Nathan's flat"))
		println(test1Analyze.findShortestPath("Kruthika's abode", "Craig's haunt"))

		val test2Analyze = new Analyzer()
		println(test2Analyze.findDistances("U", test2))
		println(test2Analyze.findShortestPath("U", "Z"))
		println(test2Analyze.findShortestPath("U", "X"))
		println(test2Analyze.findShortestPath("U", "W"))
		println(test2Analyze.findShortestPath("U", "V"))
		println(test2Analyze.findShortestPath("U", "Y"))
	}
}

class Analyzer {
	private var _edges_info = List[Map[String, Any]]()
	private var _locations = List[Location]()

	//MAKE SURE TO USE findDistances() TO FIND ALL SHORTEST PATHS BEFORE USING THIS METHOD
	def findShortestPath(start:String, end:String):Map[String, Any] = {
		val home_loc = _locations.find(temp => temp._name == start).get
		return home_loc._paths(end)
	}

	def findDistances(start:String, edges:List[Map[String, Any]]) {
		generateNodesAndEdges(edges)

		var start_edges = List[Edge]()
		start_edges :+= new Edge(start, start, 0)

		val start_loc = _locations.find(temp => temp._name == start).get
		val tenative = Map[String, Path]()
		var lowest = null.asInstanceOf[Tuple2[String, Path]]

		//first iteration
		for(edge <- start_loc._edges) {
			var start_path = new Path(start, edge._destination, edge._distance, start_edges)
			start_path._edges :+= new Edge(start, edge._destination, edge._distance)
			tenative(edge._destination) = start_path
			lowest = (edge._destination, start_path)
		}

		while(start_loc._paths.size != _locations.size) {
			//find lowest in tenative
			for((key, value) <- tenative) {
				if(value._distance < lowest._2._distance) {
					lowest = (key, value)
				}
			}

			//put lowest into confirmed, remove from tenative
			val confirmed = Map[String, Any]()
			confirmed("distance") = lowest._2._distance
			confirmed("path") = lowest._2
			start_loc._paths += (lowest._1 -> confirmed)
			tenative.remove(lowest._1)

			//populate tenative
			val just_added_loc = _locations.find(temp => temp._name == lowest._1).get

			for(edge <- just_added_loc._edges) {
				if(!start_loc._paths.contains(edge._destination)) {
					var new_path_dist = lowest._2._distance + edge._distance
					var new_path = new Path(start, lowest._1, new_path_dist, lowest._2._edges)
					new_path._edges :+= edge

					if(tenative.contains(edge._destination)) {
						//check if new path is closer than old path
						val new_path_dist = lowest._2._distance + edge._distance
						//if so, add to tenative
						if(new_path_dist < tenative(edge._destination)._distance) {
							tenative(edge._destination) = new_path
						}
					}
					//if not, create path and add to tenative
					else {
						tenative(edge._destination) = new_path
					}
				}
			}

			//set lowest to infinite distance
			val infinite_path = new Path("", "", Int.MaxValue, List[Edge]())
			lowest = ("", infinite_path)
		}
	}

	def generateNodesAndEdges(edges:List[Map[String, Any]]) {
		_edges_info = edges
		
		//create Location nodes, edges and attach
		for(edge_info <- _edges_info) {
			val start_loc_str = edge_info("startLocation").asInstanceOf[String]
			val end_loc_str = edge_info("endLocation").asInstanceOf[String]

			var start_found = _locations.find(temp => temp._name == edge_info("startLocation"))
			var start_loc = null.asInstanceOf[Location]
			start_found match {
				case Some(found) => start_loc = found
				case None => {
					start_loc = new Location(start_loc_str)
					//edge to self
					start_loc._edges :+= new Edge(start_loc_str, start_loc_str, 0)
					_locations :+= start_loc
				}
			}
			var end_found = _locations.find(temp => temp._name == edge_info("endLocation"))
			var end_loc = null.asInstanceOf[Location]
			end_found match {
				case Some(found) => end_loc = found
				case None => {
					end_loc = new Location(end_loc_str)
					//edge to self
					start_loc._edges :+= new Edge(start_loc_str, start_loc_str, 0)
					_locations :+= end_loc
				}
			}

			val distance = edge_info("distance")
			//edges go both ways
			start_loc._edges :+= new Edge(start_loc.toString(), end_loc.toString(), distance.asInstanceOf[Int])
			end_loc._edges :+= new Edge(end_loc.toString(), start_loc.toString(), distance.asInstanceOf[Int])
		}
	}
}

class Path(source:String, destination:String, distance:Int, edges:List[Edge]) {
	var _source = source
	var _destination = destination
	var _distance = distance
	var _edges = edges
	override def toString = _edges.map(_._destination).mkString("=>")
	override def clone = this
}

class Edge(source:String, destination:String, distance:Int) {
	val _source = source
	val _destination = destination
	val _distance = distance
	override def toString = "source:<" + _source + "> destination:<" + _destination + "> distance:<" + _distance + ">"
}

class Location(name:String) {
	val _name = name
	var _edges = List[Edge]()
	var _paths = HashMap[String, Map[String, Any]]() //ex: ["Mark's crib" => ["distance" => Int, "path" => List[Edge]]]
	override def toString = name
}