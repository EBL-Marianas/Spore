/**
* Name: SPORE (Spatial Planning for Optimal Rewilding Efforts)
* Author: Hugo Thierry
* Description: The SPORE models aim in identifying and ranking management units that will provide a specific ecosystem service through rewilding
* Tags: 
*/

model SPORE_Clust

global{
	
//////// Declare all parameters and inputs	
	
	file landscapeGIS  <- shape_file('../includes/GuamVegMap.shp'); 	// Import GIS data
	file landscapeRaster <- grid_file('../includes/asciiguamfinal.txt');					// Raster input to define cell size
	file landcoverCSV <-  file('../includes/GuamCovers.csv');						// CSV import to define land covers
	file roadsLandscape <- shape_file('../includes/AllRoadsBuff.shp');					// Import Roads Shapefile
	geometry shape<- envelope(landscapeGIS);										// The landscape will have the same shape as the landscape GIS file
		
	// Use this list to output all management units, their ranks and parameters
	list<ManagementUnit> allUnitsMixed;	
	list<ManagementUnit> top10PercentMixed;
	list<ManagementUnit> bestMixed;
	list<ManagementUnit> goodMixed;
	list<ManagementUnit> notAsGoodMixed;
	list<ManagementUnit> poorMixed;	
		
	list<ManagementUnit> allUnitsDev;
	list<ManagementUnit> top10PercentDev;
	list<ManagementUnit> bestDev;
	list<ManagementUnit> goodDev;
	list<ManagementUnit> notAsGoodDev;
	list<ManagementUnit> poorDev;				
	
	list<ManagementUnit> allUnitsNonDev;
	list<ManagementUnit> top10PercentNonDev;
	list<ManagementUnit> bestNonDev;
	list<ManagementUnit> goodNonDev;
	list<ManagementUnit> notAsGoodNonDev;
	list<ManagementUnit> poorNonDev;	

//////////////////	Initialize the model
		
	init{
		
	/// Create The Entities Modeled
		do create_landcovers;						// Create landcover entities
		write "Landcovers created";					// Helps track the progression of the model on a cluster
		do define_cell_cover;						// Define the landcover of each cell
		write "Cover assigned to cells";			// Helps track the progression of the model on a cluster
		do create_roads;							// Create road entities
		write "Roads created";		
	/////////////////////////////////
	
		
	/// Calculate Service Score (Ecosystem Service)  and Update the output
		ask LandCell where ((each.cellCover!=nil) and ((each.cellCover.vegNb=10) or (each.cellCover.vegNb=11) or (each.cellCover.vegNb=13) or (each.cellCover.vegNb=14) or (each.cellCover.vegNb=17))) parallel:true{
			// This is done for Native Forest, Mixed introduced forest, Vitex forest, Leucaena thicket and Hibiscus thicket
			do calculate_service_score;	
		}
		write "forestScoresCalculated";				// Helps track the progression of the model on a cluster						
	////////////////////////////////
	
	/// For each cell that could host starlings, estimate if habitat requirements are met
		ask LandCell where(each.cellCover!=nil and each.cellCover.vegNb>=0) parallel:true{
			do estimate_forest_resource;
		}
		write "habitatsEvaluated";		
	/////////////////////////////////////////		
		do print_csv;
		write "Scores Printed";
										
		do aggregate_dev;
		do aggregate_nonDev;
		write "Management areas created";

		do divide_with_roads;
		do calculate_mng_score;
	
		do rankManagementUnits;
		do qualityCategoriesMixed;
		do qualityCategoriesDev;
		do qualityCategoriesNonDev;
		write "Management areas ranked";
		
	///////////////////////////////
	
	/// Call Outputs
		do print_map_management;

	}	
	
	
	/// Create Landcovers
	action create_landcovers{
		matrix landcoverMatrix <-matrix(landcoverCSV); // Create a matrix from the csv data
		loop x from: 0 to: ((landcoverMatrix column_at(0)) count (each!='')-1){ // Loop over each row of this matrix
		// Each loop creates a landcover 
			create Landcover number:1 returns: newCover{
				name <- string(landcoverMatrix[0,x]);	// Read the name				
				color <- rgb([int(landcoverMatrix[1,x]),int(landcoverMatrix[2,x]),int(landcoverMatrix[3,x])]); // Read the color in RGB
				vegNb <- int(landcoverMatrix[4,x]);
			}					
		}		
	}
	/////////////////////////////
	
	////// Define the landcover of each cell
	action define_cell_cover{
		ask LandCell where(each.grid_value>=(-6)){
			cellCover <- any(Landcover where (each.vegNb=grid_value));
			if cellCover.vegNb=7 or cellCover.vegNb=8{
				type<-"dev";
			}
			else{
				type<-"nonDev";
			}
		}
		ask LandCell where(each.grid_value<(-6)){
			cellCover <- any(Landcover where (each.vegNb=-5));
		}
	}
	////////////////////////////
	
	/////// Create Roads
	action create_roads{
		create Road from: roadsLandscape with: [iD::int(read("FID"))];
	}
	
	/////////////////////////////////////////////////
	
	///// Aggregate cells into Developed Areas
	action aggregate_dev{
		loop while: !empty(LandCell where((!each.aggregated)and(each.type="dev")and(each.reintroductionScore>0))){
			ask one_of(LandCell where((!each.aggregated)and(each.type="dev") and (each.reintroductionScore>0))){
				list<LandCell> new_neigh<-self.neighbors where((each.type="dev") and (each.reintroductionScore>0))+self;
				list<LandCell> neigh<-copy(new_neigh);
				ask new_neigh{
					aggregated<-true;
				}
				
				loop while: !empty(new_neigh){
					new_neigh <- remove_duplicates(neigh accumulate(each.neighbors where((each.type="dev") and (each.reintroductionScore>0) and (!each.aggregated))));
					add new_neigh to: neigh all:true;
					
					if (length(neigh) !=length(remove_duplicates(neigh))){
					}
					
					ask new_neigh {
						aggregated<-true;
					}
				}
				
				create ManagementUnit with:[shape::union(neigh)]{
					type <- "dev";
					cellsInUnit<-neigh;				
					size <- length(cellsInUnit) * 900;
					if  length(cellsInUnit)<10{ // less than 1 hectare
						do die;
					}
				}
			}
		}
	}
	//////////////////////////////
	
	///// Aggregate cells into Non Developed Areas
	action aggregate_nonDev{
		loop while: !empty(LandCell where((!each.aggregated)and(each.type="nonDev") and (each.reintroductionScore>0))){
			ask one_of(LandCell where((!each.aggregated)and(each.type="nonDev") and (each.reintroductionScore>0))){
				list<LandCell> new_neigh<-self.neighbors where((each.type="nonDev") and (each.reintroductionScore>0))+self;
				list<LandCell> neigh<-copy(new_neigh);
				ask new_neigh{
					aggregated<-true;
				}
				
				loop while: !empty(new_neigh){
					new_neigh <- remove_duplicates(neigh accumulate(each.neighbors where((each.type="nonDev") and (each.reintroductionScore>0)and (!each.aggregated))));
					add new_neigh to: neigh all:true;
					
					if (length(neigh) !=length(remove_duplicates(neigh))){
					}
					
					ask new_neigh {
						aggregated<-true;
					}
				}
				
				create ManagementUnit with:[shape::union(neigh)]{
					type <- "nonDev";
					cellsInUnit<-neigh;
					size <- length(cellsInUnit) * 900;					
					if  length(cellsInUnit)<10{ // less than 1 hectare
						do die;
					}
				}
			}
		}
	}
	//////////////////////////////
	
	///// Divide Units using Roads
	action divide_with_roads{
		ask ManagementUnit{
			geometry new_shape <- copy(self);
			loop r over: Road where (each.shape partially_overlaps self){
				new_shape <- new_shape - r;
			}
			create ManagementUnit from: new_shape.geometries{
				type<-myself.type;
			}
			do die;					
		}	
	}
	////////////////////////////

	action calculate_mng_score{
		ask ManagementUnit{
			cellsInUnit <- LandCell where(centroid(each) overlaps self);
			if length(cellsInUnit)<11{
				do die;
			}
			ask cellsInUnit{
				myself.CellsUnitScore<-myself.CellsUnitScore+reintroductionScore;
			}
			CellsUnitScore<-CellsUnitScore/(length(cellsInUnit));
			finalManagementScore<-CellsUnitScore;
		}
	}

	
	//// Rank all management units
	action rankManagementUnits{
		ask ManagementUnit{
			allUnitsMixed<- allUnitsMixed + self;
			if type="dev"{
				allUnitsDev <- allUnitsDev + self;				
			}
			if type="nonDev"{
				allUnitsNonDev <- allUnitsNonDev + self;					
			}
		}
		allUnitsMixed <- allUnitsMixed sort_by(- each.finalManagementScore);
		allUnitsDev <- allUnitsDev sort_by(- each.finalManagementScore);
		allUnitsNonDev <- allUnitsNonDev sort_by(- each.finalManagementScore);				
	}
	/////////////////////////////////////
	
	//// Categorize the management Unit in different qualities
	action qualityCategoriesMixed{
		int totalSize <-0;
		ask allUnitsMixed{
			totalSize<- totalSize + size;
		}
		float tenThreshold <-totalSize*0.1 ;
		float twentyFiveThreshold <-totalSize*0.25 ;
		float fiftyThreshold <-totalSize*0.5 ;
		float seventyFiveThreshold <-totalSize*0.75 ;
		int sizeIncrement <- 0;
		ask allUnitsMixed{
			if sizeIncrement < tenThreshold{
				top10PercentMixed <- top10PercentMixed + self;
				sizeIncrement<-sizeIncrement + size;
				rankAll <- "10%";					
			}
			else if sizeIncrement < twentyFiveThreshold{
				bestMixed <- bestMixed + self;
				sizeIncrement<-sizeIncrement + size;
				rankAll <- "25%";			
			}
			else if sizeIncrement < fiftyThreshold{
				goodMixed <- goodMixed + self;
				sizeIncrement<-sizeIncrement + size;
				rankAll <- "50%";				
			}
			else if sizeIncrement < seventyFiveThreshold{
				notAsGoodMixed <- notAsGoodMixed + self;
				sizeIncrement<-sizeIncrement + size;
				rankAll <- "75%";				
			}
			else{
				poorMixed <- poorMixed + self;
				sizeIncrement<-sizeIncrement + size;
				rankAll <- "100%";				
			}
		}
	}
	
	action qualityCategoriesDev{
		int totalSize <-0;
		ask allUnitsDev{
			totalSize<- totalSize + size;				
		}
		float tenThreshold <-totalSize*0.1;
		float twentyFiveThreshold <-totalSize*0.25;
		float fiftyThreshold <-totalSize*0.5;
		float seventyFiveThreshold <-totalSize*0.75;
		int sizeIncrement <- 0;
		ask allUnitsDev{
			if sizeIncrement < tenThreshold{
				top10PercentDev <- top10PercentDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "10%";				
			}
			else if sizeIncrement < twentyFiveThreshold{
				bestDev <- bestDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "25%";				
			}
			else if sizeIncrement < fiftyThreshold{
				goodDev <- goodDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "50%";				
			}
			else if sizeIncrement < seventyFiveThreshold{
				notAsGoodDev <- notAsGoodDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "75%";				
			}
			else{
				poorDev <- poorDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "100%";				
			}
		}						
	}
	
	action qualityCategoriesNonDev{
		int totalSize <-0;
		ask allUnitsNonDev{
			totalSize<- totalSize + size;					
		}
		float tenThreshold <-totalSize*0.1 ;
		float twentyFiveThreshold <-totalSize*0.25 ;
		float fiftyThreshold <-totalSize*0.5 ;
		float seventyFiveThreshold <-totalSize*0.75 ;
		int sizeIncrement <- 0;
		ask allUnitsNonDev{
			if sizeIncrement < tenThreshold{
				top10PercentNonDev <- top10PercentNonDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "10%";				
			}
			else if sizeIncrement < twentyFiveThreshold{
				bestNonDev <- bestNonDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "25%";			
			}
			else if sizeIncrement < fiftyThreshold{
				goodNonDev <- goodNonDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "50%";				
			}
			else if sizeIncrement < seventyFiveThreshold{
				notAsGoodNonDev <- notAsGoodNonDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "75%";				
			}
			else{
				poorNonDev <- poorNonDev + self;
				sizeIncrement<-sizeIncrement + size;
				rankType <- "100%";				
			}
		}						
	}
	
	//////////////////////////////////////////
	
	action print_map_management{
		ask ManagementUnit{
			save species_of(self) to:"mgmntSali30.shp" type:"shp" with: [name::"namAg", location::"locAg",size::"size",rankType::"mngUnit",rankAll::"mngAll",type::"mngType",finalManagementScore::"UniSco"];// crs:"EPSG:4326";			
		}	
	}
	
	action print_csv{
		ask LandCell where((each.serviceScore>0)or(each.habitatRequirement=true)or(each.reintroductionScore>0)){
			save [name,type, location,serviceScore,grid_value,habitatRequirement,reintroductionScore,cellCover] to: "SaliCSV30.csv" type:"csv" rewrite:false;						
		}		
	}		
}		
/////////////////////////////////

/// Define the different species of the model

	//////// Landcover Species
species Landcover{
	rgb color;	// RGB Color of the landcover when visualized
	int vegNb;
}
	/////////////////////
	
	///////// Road Species
species Road{
	int iD;
	aspect base{draw shape color:rgb(0,0,0);}
}	

	//////// LandCell Species
grid LandCell file:landscapeRaster neighbors:4{ //cell_width:50 cell_height:50
	Landcover cellCover; // Landcover assigned to the cell
	float serviceScore <- 0.0; // Forest score of the cell
	float reintroductionScore <- 0.0; // Reintroduction score of the cell
	string type; // dev or nonDevt;
	 
	bool habitatRequirement <-false; // Is there enough forest resource around me to feed the birds
	
	geometry bufferMaxDispersal <- circle(500#m);
	geometry bufferHomerange <- circle(1083#m);
	
	bool aggregated <- false;

	// CALCULATE SERVICE SCORES FOR SEED DISPERSAL //
	action calculate_service_score{
		list<LandCell> nativeNeighborsDispersal<- LandCell where((each.cellCover!=nil) and (each.cellCover.vegNb=14) and (centroid(each) overlaps bufferMaxDispersal));
		list<LandCell> totalNeighborsDispersal<- LandCell where((each.cellCover!=nil) and (centroid(each) overlaps bufferMaxDispersal));
		int totNatNeigh;
		float distNearestNeighborDispersal; // distance of that nearest native neighbor within dispersal range				
		if nativeNeighborsDispersal!=nil{
			LandCell nearestNeighborDispersal <- nativeNeighborsDispersal closest_to self;
			if nearestNeighborDispersal!=nil{
				totNatNeigh<-length(nativeNeighborsDispersal);						
				distNearestNeighborDispersal<- sqrt((self.location.x-nearestNeighborDispersal.location.x)^2+(self.location.y-nearestNeighborDispersal.location.y)^2);				
			}
			else{
				distNearestNeighborDispersal<-100000.0;
				totNatNeigh<-0;						
			}		
		}
		else{
			distNearestNeighborDispersal<-100000.0;
			totNatNeigh<-0;
		}
		if cellCover.vegNb=14 {
			serviceScore<- (totNatNeigh/length(totalNeighborsDispersal))*(1-(pnorm(ln(distNearestNeighborDispersal),4.534042,0.9710314)));				
		}		
		if cellCover.vegNb=10 or cellCover.vegNb=11 {
			serviceScore<- (totNatNeigh/length(totalNeighborsDispersal))*(1-(pnorm(ln(distNearestNeighborDispersal),4.534042,0.9710314)))*0.6;				
		}
		if cellCover.vegNb=13 or cellCover.vegNb=17 {
			serviceScore<- (totNatNeigh/length(totalNeighborsDispersal))*(1-(pnorm(ln(distNearestNeighborDispersal),4.534042,0.9710314)))*0.8;				
		}
		if serviceScore < 0{
			serviceScore <- 0.0;
		}
	}
	
	// CALCULATE REINTRODUCTION SCORES FOR DOVES //
	action calculate_reintroduction_score{
		list<LandCell> cellsInHomerange <- LandCell where(centroid(each) overlaps bufferHomerange);
		list<LandCell> forestInHomerange <- cellsInHomerange where(each.serviceScore > 0);	
		ask forestInHomerange{
			float distToCell <- sqrt((myself.location.x-location.x)^2+(myself.location.y-location.y)^2);
			if distToCell>1083{
				distToCell <- 1083.0;
			}
			myself.reintroductionScore<- myself.reintroductionScore + (((1083-distToCell)/1083)*serviceScore);
		}
		reintroductionScore <- reintroductionScore/(length(cellsInHomerange));
	}
	
	
	action estimate_forest_resource{
		list<LandCell> cellsInHomerange <- LandCell where(centroid(each) overlaps bufferHomerange);		
		list<LandCell> nativeForestAround <- LandCell where((each.cellCover.vegNb=14) and (centroid(each) overlaps bufferHomerange));
		int forestArea <- length(nativeForestAround)*900; // in square meters
		int areaAround <- length(cellsInHomerange)*900; // in square meters
		if forestArea > (areaAround * 0.1) { // 4	0% of the homerange must be native forest
			habitatRequirement <- true;
			do calculate_reintroduction_score;
		}
		else{
			habitatRequirement <- false;
			reintroductionScore <-0.0;
		}
	}
		
	
	aspect serviceScore {
		draw shape color:cellCover.color depth: serviceScore*1000;
	}
	
	aspect reintroductionScore {
		draw shape color:cellCover.color depth: reintroductionScore*1000;		
	}
	
}	

//// ManagementUnits
species ManagementUnit{
	string type;								// Forest or Non Forest
	string rankAll;								// Rank amongst all Management Unit
	string rankType;							// Rank amongst Units of same type
	list<LandCell> cellsInUnit;					// List of a cells in the Unit
	int size;									// Size
	float CellsUnitScore <- 0.0;				// Temporary management score
	float finalManagementScore<- 0.0;			// Final management score of the cell
	
	aspect default{
		draw shape  depth:finalManagementScore*1000 color:rgb(rnd(255),rnd(255),rnd(255));
	}
}
//////////////////////////////////////

experiment Test_Batch type: batch repeat:1 keep_seed: false  until: (step>1){

}