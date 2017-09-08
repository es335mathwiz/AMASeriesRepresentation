package forImport;
//Wed 6 Sep 2017 15:36:10
import libsvm.PRECOMPUTED;
public class dwell extends PRECOMPUTED {
public double [] testExpKern(double[] xNow)   {
   double [] theExpVals = new double[5];
double okay1;


okay1=0.5*xNow[0];
theExpVals[0]=okay1;
theExpVals[1]=okay1;
theExpVals[2]=okay1;
theExpVals[3]=0.;
theExpVals[4]=1.*xNow[0];
   return(theExpVals);
} 
public double [] testKern(double[] xNow)   {
   double [] theVals = new double[5];
double okay1;


okay1=0.5*xNow[0];
theVals[0]=0.+okay1;
theVals[1]=-0.03*xNow[1]+okay1;
theVals[2]=0.03*xNow[1]+okay1;
theVals[3]=0.;
theVals[4]=0.+1.*xNow[0];
   return(theVals);
} 
public double [][] xVals(){
double[][]xRes=
{{0.5, 0.}, {0.5, -0.03}, {0.5, 0.03}, {0., 0.}, {1., 0.}};
return(xRes);
}
}


