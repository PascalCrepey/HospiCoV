#include <algorithm>
#include <math.h> 
#include <iomanip>

#define ARMA_USE_BOOST
#include <RcppArmadillo.h>

using namespace std;
using namespace Rcpp;

#ifndef M_PI //not there on windows
#define M_PI 3.1415927 //...
#endif

#define days_in_year 364

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_odeiv.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppGSL)]]

/* The type of container used to hold the state vector */
typedef arma::mat state_type;

class SARSCoVMod{
  int cpt_run;
  
public:
  //number of compartments
  static const int ncol=4;
  // Constructors
  SARSCoVMod(){}
  SARSCoVMod(List parameters){
    init(parameters);
  }
  int nage;
  arma::vec f_S_E, f_E_I, f_I_R;
  arma::vec newI, N;
  arma::mat contact;
  arma::vec susceptibility;
  double progression, removal, beta;
  

  void init(List parameters){
    cpt_run=0;
    //number of age-groups
    nage=parameters["nage"];
    // progression from E to I
    progression = parameters["progression"];
    // removal from I to R
    removal = parameters["removal"];
    // probability of transmission
    beta = parameters["beta"];
    // susceptibility
    susceptibility=as<arma::colvec>(parameters["susceptibility"]);

    //contact matrix
    contact=as<arma::mat>(parameters["contact"]);

    f_S_E=arma::vec(nage);
    f_E_I=arma::vec(nage);
    f_I_R=arma::vec(nage);

  }
  
  /*
   * dxdt computation
   */
  void run(const state_type &states_, state_type &dstates, const double time){ 
    //count the number of function call
    cpt_run++;
    
    state_type states=const_cast<state_type&>(states_);
    
    /*
    * Load state in the right variables 
    * using: vec(*aux_mem, number_of_elements, copy_aux_mem = true, strict = true) 
    * states are in the order S E I R 
    */
    //State vectors  
    arma::vec S(states.colptr(0), nage, false, true);
    arma::vec E(states.colptr(1), nage, false, true);
    arma::vec I(states.colptr(2), nage, false, true);
    arma::vec R(states.colptr(3), nage, false, true);
    
    //dxdt vectors
    arma::vec dS(dstates.colptr(0), nage, false, true);
    arma::vec dE(dstates.colptr(1), nage, false, true);
    arma::vec dI(dstates.colptr(2), nage, false, true);
    arma::vec dR(dstates.colptr(3), nage, false, true);
    
    N=S+E+I+R;

    
    f_S_E =  ((beta * contact * I) / N) % S % susceptibility;
    
    dS = -f_S_E ;
    
    f_E_I = E * progression;
    
    dE = f_S_E - f_E_I ; 
    
    f_I_R = I * removal;
    dI = f_E_I - f_I_R;
    
    dR = f_I_R;
    
    newI = f_E_I;
    
  }
  
  //initialize model states
  arma::mat initModelStates(arma::mat init){ 
    arma::mat initStates=arma::mat(init);
    return(initStates);
  }
};

int run_wrapper(double t,  const double y[], double f[],void *F_ ){
  SARSCoVMod *F=(SARSCoVMod*)F_;
  //export y and f to states and dstates

  state_type states(const_cast<double*>(y), F->nage, F->ncol, false, true);
  state_type dstates(f, F->nage, F->ncol, false, true);
  
  //run the model for one step
  F->run(states, dstates, t);
  
  return GSL_SUCCESS;
}

void integrate_model(SARSCoVMod &F, arma::mat init, arma::mat &res, int start, int end){
  int size=init.n_elem;
  int shift=0;
  double t, t_next;		// current and next independent variable 
  double tmin, tmax, delta_t;	// range of t and step size for output 
  //cout.precision(8);
  //cout<<"on commence"<<endl;
  const gsl_odeiv_step_type * T 
    = gsl_odeiv_step_rkck;
  //= gsl_odeiv_step_rkf45;
  //= gsl_odeiv_step_rk4;
  //end=21;
  gsl_odeiv_step * s 
    = gsl_odeiv_step_alloc (T, size);
  
  //   
  //    * step function needs to be outside of the object
  //    * a pointer to the object can be passed on as a parameter
  //    

  gsl_odeiv_control * c 
    = gsl_odeiv_control_y_new (1e-2, 0.0);
  gsl_odeiv_evolve * e 
    = gsl_odeiv_evolve_alloc (size);
  
  arma::vec y_=F.initModelStates(init);

  double *y=y_.memptr();
  double h = 1e-6;		
  
  gsl_odeiv_system my_system;	/* structure with the rhs function, etc. */
    /* load values into the my_system structure */
    my_system.function = run_wrapper;	/* the right-hand-side functions dy[i]/dt */
    my_system.jacobian = NULL;	/* the Jacobian df[i]/dy[j] */
    my_system.dimension = size;	/* number of diffeq's */
    my_system.params = &F;	/* parameters to pass to rhs and jacobian */

    int i=0;
    tmin = start;			// starting t value 
    tmax = end;			// final t value 
    delta_t = 1.;
    t = tmin;
    res.col(i)[0]=t;
    copy(y,y+size,res.colptr(i)+1);

    //put the population size
    state_type myinit(const_cast<double*>(y), F.nage, F.ncol, false, true);
    arma::vec popsize=sum(myinit,1);
    copy(popsize.begin(), popsize.end(), res.colptr(i)+size+1+F.nage);

    for (t_next = tmin + delta_t; t_next <= tmax; t_next += delta_t)
    { 
      while (t < t_next)
      {
        gsl_odeiv_evolve_apply (e, c, s, 
                                &my_system, 
                                &t, t_next, &h,
                                y);
      }
      i++;

      res.col(i)[0]=t_next;
      shift=1;
      copy(y,y+size,res.colptr(i)+1);
      //newI
      shift+=size;
      copy(F.newI.begin(), F.newI.end(), res.colptr(i)+shift);
      
      //N
      shift+=F.nage;
      copy(F.N.begin(), F.N.end(), res.colptr(i)+shift);
      
    }
    /* all done; free up the gsl_odeiv stuff */
    gsl_odeiv_evolve_free (e);
    gsl_odeiv_control_free (c);
    gsl_odeiv_step_free (s);
    
}


//' Function that initialize (or reinitialize) the initial state of the model
//' @param params list of various model parameters
//' @param init initial state of the model
//'
//' @return a matrix of the result
//' @export
// [[Rcpp::export]] 
arma::mat engine_run(List params, arma::mat init){

  SARSCoVMod F;

  F = SARSCoVMod(params);
  
  int tstart1 = 1;

  int tend1 = params["nbDays"];

  arma::mat res=arma::zeros<arma::mat>(init.n_elem+2*F.nage+1, tend1);
//cout<<"bouh go for integration"<<endl;
  integrate_model(F, init, res, tstart1, tend1);

  return(res);
}
