public class proof{

    public boolean A = false;
    public boolean B = true;
    
    public proof(){
	boolean A = this.A;
	boolean B = this.B;
    }
    
    public boolean getA(){
	System.out.println("A has been evaluated");
	return this.A;
    }
    public boolean getB(){
	System.out.println("B has been evaluated");
	return this.B;
    }
        
    
    public static void main(String [] arg){
	boolean A = false;
	boolean B = true;
	proof mine = new proof();
	boolean too;
	System.out.println("Evaluation 1: getA() || B");
	too = mine.getA() || B ;
	System.out.println("Evaluation 1: " + too + "\n");
	System.out.println("\nEvaluation 2: B || getB()");
	too = B || mine.getA();
	System.out.println("Evaluation 2: " + too);
	
    }   
}
