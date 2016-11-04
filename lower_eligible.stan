functions {
  // Calculate the boundaries for zeta for unobserved zeta 
    /** 
      * return: lower bound and upper bound for zeta for corresponding range of coinsurance rate, also return the value of 
      *         zeta at which there is bunching and the effective coinsurance at zeta = 1, and also return the income adjustment for zeta = 1
      */
    vector[] bound_z1(real k1, real k2, real k3, real mbar, vector oop, int K) {
      vector[K] output[5]; 

      output[1] = 1  ./  (oop*inv(mbar) + (1 - k1)); 
      output[2] = 1  ./  ( (oop + mbar*(k2 -k1)) * inv(mbar*(k2 -k1)*inv(k2 -k3)) + (1-k2) ); 
      output[3] = mbar  ./  (oop + mbar*(1 - k1)); 

      for (k in 1:K) {
        if (output[1][k] > 1) {
          output[1][k] = 1; 
          output[2][k] = 1; 
          output[4][k] = k1;  
          output[5][k] = 0; 
        }
        else if (output[2][k] > 1) {
          output[2][k] = 1; 
          output[4][k] = k2;
          output[5][k] = mbar*(k2-k1);  
        }
        else {
          output[4][k] = k3; 
          output[5][k] = 0; 
        }
      }
      return output; 
    }

    vector[] bound_z2(real k1, real k2, real mbar, vector oop, int K) {
      vector[K] output[4]; 

      output[1] = 1  ./  (oop*inv(mbar) + (1 - k1)); 
      output[2] = 1  ./  (oop*inv(mbar) + (1 - k2)); 
      output[3] = mbar  ./  (oop + mbar*(1 - k1)); 

      for (k in 1:K) {
        if (output[1][k] > 1) {
          output[1][k] = 1; 
          output[2][k] = 1;  
          output[4][k] = k1; 
        }
        else if (output[2][k] > 1) {
          output[2][k] = 1; 
          output[4][k] = k1; 
        }
        else {
          output[4][k] = k2; 
        }
      }
      return output; 
    }
    /** 
      * return: z_l: for 0 <= z_l: effective coinsurance rate is k1 
      *         z_m
      *         z_u: for z_m <= z <= z_u: effective coinsurance rate is k2, and 1 >= z >= z_u is k2 
      *         p_1: the effective coinsurance rate at zeta = 1
      *         add_y: the additional income at zeta = 1
      */
    vector[] bound_z3(real k1, real k2, real k3, real mbar, real mbar2, vector oop, int K) {
      vector[K] output[6]; 

      output[1] = 1  ./  (oop*inv(mbar) + (1 - k1)); 
      output[2] = 1  ./  (oop*inv(mbar) + (1 - k2)); 
      output[3] = 1  ./  (oop*inv(mbar2) + (1 - k2)); 
      output[4] = mbar  ./  (oop + mbar*(1 - k1)); 

      for (k in 1:K) {
        if (output[1][k] > 1) {
          output[1][k] = 1; 
          output[2][k] = not_a_number();
          output[3][k] = not_a_number(); 
          output[4][k] = not_a_number();   
          output[5][k] = not_a_number(); 
          output[6][k] = not_a_number(); 
        }
        else if (output[2][k] > 1) {
          output[2][k] = 1; 
          output[3][k] = not_a_number(); 
          output[5][k] = k2; 
          output[6][k] = 0; 
        }
        else if (output[3][k] > 1) {
          output[3][k] = 1; 
          output[5][k] = k2; 
          output[6][k] = 0; 
        }
        else {
          output[5][k] = k3; 
          output[6][k] = - mbar*(k2 - k3); 
        }
      }
      return output; 
    }


  // Calculate premium for each year 
      /**
      * This function calculates the premium for health insurance 
      * 
      * @param baseprice baseprice (first unit of health insurance, depending in area )
      * @param year 
      * @param Num Number of health insurance purchase 
      * @param hhtype Whether the household needs to be fully enrolled to receive discount, 
      *         or it can enroll partially, or no discount at all 
      * @param hhsize Household size (except members with compulsory/beneficiaries)
      * 
      * @return Premium at that bundle 
      */
      real Premium_fun(int N_vol, int year, real baseprice, int hhtype, int hhsize) {
        real output; 
        if (year == 2004) {
          if (N_vol == 1) {
            output = baseprice; 
          }
          else {
            output = baseprice + baseprice * 0.95 * (N_vol - 1); 
          }
        }
        else if ((year <= 2008) && (year > 2004)) {
          if (N_vol <= 2){
            output = baseprice*N_vol ;
          }
          else if (N_vol == 3){
            output = baseprice*2 + baseprice * 0.9 ; 
          }
          else if (N_vol >= 4){
            output = baseprice*2 + baseprice * 0.9 + baseprice * (N_vol - 3) * 0.8;
          }
        }
        else {
          // Note that only if your family is in the agriculture sector or near-poor that you have
          // to have everyone in the household paying for health insurance in order to receive discount
          // If on the other hand, the household has eligible-for-compulsory members in the household, 
          // then no need to have everyone committing to having health insurance to receive discount. 
          // Self employed household does not receive any discount (Kinda odd)
          if (hhtype == 1) {// Agricultural sector/near poor 
            if (N_vol == hhsize) {
              if (N_vol == 1) {
                output = baseprice;
              }
              else if (N_vol == 2) {
                output = baseprice  + baseprice * 0.9;
              }
              else if (N_vol == 3) {
                output = baseprice + baseprice * 0.9 + baseprice * 0.8; 
              }
              else {
                output = baseprice + baseprice * 0.9 + baseprice * 0.8 + baseprice * (N_vol - 3) * 0.7;
              }
            }
            else {
              output = baseprice * N_vol;
            }
          }
          else if (hhtype == 2) { // having compulsory members in the household 
              if (N_vol == 1) {
                output = baseprice;
              }
              else if (N_vol == 2) {
                output = baseprice  + baseprice * 0.9;
              }
              else if (N_vol == 3) {
                output = baseprice + baseprice * 0.9 + baseprice * 0.8; 
              }
              else {
                output = baseprice + baseprice * 0.9 + baseprice * 0.8 + baseprice * (N_vol - 3) * 0.7;
              }
          }
          else if (hhtype == 3) { // Self employed
            output = baseprice * N_vol; 
          }
        }
      return output;  
      }

  // Find the factorial 
      /** 
      * @param N 
      * 
      * @return N! 
      */
      int factorial(int N); 

      int factorial(int N) {
        if (N == 0) {
          return 1; 
        }
        else if (N == 1) {
          return 1; 
        }
        else {
          return N* factorial(N-1); 
        }
      }

  // Find all permutations of a list 
      /** 
      * @param N length of list 
      * 
      * @return all permutations of the list
      */
      int[,] permutations(int N); 

      int[,] permutations(int N) {
        if (N == 1) {
          return rep_array(N, 1, 1); 
        }
        else {
          int output[factorial(N), N]; 

          for (i in 1:N) {
            int startind; 
            int endind; 
            startind = (i-1)*factorial(N-1) + 1; 
            endind = i*factorial(N-1); 
            if (i == 1) {
              output[startind:endind,1] = rep_array(N,factorial(N-1)); 
              output[startind:endind,2:] = permutations(N-1); 
            }
            else if (i == N) {
              output[startind:endind,N] = rep_array(N,factorial(N-1)); 
              output[startind:endind,1:N-1] = permutations(N-1); 
            }
            else {
              output[startind:endind,i] = rep_array(N,factorial(N-1)); 
              output[startind:endind,1:i-1] = permutations(N-1)[,1:i-1];
              output[startind:endind,i+1:] = permutations(N-1)[,i:];
            }
          }
          return output;  
        }
      }
  
  // N choose k 
    int choose(int n, int k) {
      return factorial(n)/factorial(k)/factorial(n-k); 
    }

  // Code n choose k
    int[,] subset(int n, int k, int[] setn) ; 

    int[,] subset(int n, int k, int[] setn) {
      if (k == 1) {
        int output[n,1]; 
        for (i in 1:n) {
          output[i,1] = setn[i];
        }
        return output; 
      }
      else if ((k == n) && (k >1)) {
        int output[1,n];
        for (i in 1:n) {
          output[1,i] = setn[i]; 
        }
        return output; 
      }
      else {
        int output[choose(n,k), k];
        int count; 
        count = 1; 
        for (i in 1:(n-k+1)) { 
          int subsetn[n-i]; 
          # ########print("i =", i);
          for (j in i+1:n) {
            subsetn[j - i] = setn[j];  
          }
          # ########print("subsetn = ", subsetn); 
          # ########print("subset =", subset(n-i, k-1, subsetn)); 
          output[count:(count + choose(n-i,k-1) - 1),1:(k-1)] = subset(n-i, k-1, subsetn);
          for (kk in count:(count + choose(n-i, k-1) -1)) {
            output[kk,k] = setn[i];
          }
          count = count + choose(n-i, k-1); 
        }
        return output; 
      }
    }    

  // Return the bound for voluntary member being considered  
    /** 
      * @param vec_prem: an array of vector for each level of premium
      * @param vec_draw: value of other household members 
      * @param K: length of index
      * @param 
      * @param n: position of current member 
      * @param index: index of the vector 
      * 
      * return a vector of the minimum of all elements 
      */ 
    vector vol_vec(real[,] vec_prem, vector[] vec_draw, int K, int n) {
      vector[K] output; 
      vector[n] temp; 
      vector[n] temp2;

      for (k in 1:K) {
        for (j in 1:(n-1) ) {
          temp[j] = vec_draw[j][k]; 
        }
        temp = sort_desc(temp); 
        temp2[1] = vec_prem[k,1];
        for (j in 2:n) {
          temp2[j] = vec_prem[k,j] - sum(temp[1:(j-1)]); 
        }
        output[k] = min(temp2); 
      }
      return output; 
    }

  // Return the bound for non-voluntary member being considered  
    /** 
      * @param vec_prem: an array of vector for each level of premium
      * @param vec_draw: value of other household members 
      * @param K: length of index
      * @param 
      * @param n: position of current member 
      * @param index: index of the vector 
      * 
      * return a vector of the minimum of all elements 
      */ 
    vector novol_vec(real[,] vec_prem, vector[] vec_draw, int K, int n, int[] index) {
      vector[K] output; 
      vector[n] temp; 
      vector[n] temp2;

      for (k in 1:K) {
        for (j in 1:(n-1) ) {
          temp[j] = vec_draw[j][index[k]]; 
        }
        temp = sort_asc(temp); 
        temp2[1] = vec_prem[1][k];
        for (j in 2:n) {
          temp2[j] = vec_prem[j][k] - sum(temp[1:(j-1)]); 
        }
        output[k] = max(temp2); 
      }
      return output; 
    }
}

data {
  int Ndata; 
  int Nind ;
  int Nhh ;
  vector[Ndata] year04_r; 
  vector[Ndata] year06_r; 
  vector[Ndata] year10_r; 
  vector[Ndata] year12_r;
  int X_ind_wo_dim ;
  int X_hh_dim ;
  int X_hh_wo_dim ;
  matrix[Nind, X_ind_wo_dim] X_ind_wo ;
  matrix[Nhh, X_hh_dim] X_hh ;
  matrix[Nhh, X_hh_wo_dim] X_hh_wo ;
  vector[Ndata] Baseprice ;
  vector[Ndata] Baseprice_s ;
  vector[Ndata] M_expense ;
  vector[Ndata] Income ;
  int HHid[Ndata];
  int IVid[Ndata]; 
  int sick_dummy[Ndata];
  vector[Ndata] tot_cost ;
  vector[Ndata] eff_coins ;
  real unit_inc; 
  int N_zeta_0;
  int N_zeta_1;
  vector[Ndata] zeta_observed; 
  int zeta_observed_0[N_zeta_0]; 
  int zeta_observed_1[N_zeta_1];
  int N_zeta_observed;
  int zeta_observed_index[N_zeta_observed];
  int Nbc; 
  int bc[Nbc]; 
  int N_com[Nhh]; 
  int N_bef[Nhh]; 
  int N_std_wo_ins[Nhh]; 
  int N_std_w_ins[Nhh]; 
  int N_vol[Nhh]; 
  int N_noins[Nhh]; 
  int Year[Nhh]; 
  int HHtype[Nhh]; 
  vector[Nhh] Yhh; 
  int Nn_1s; 
  int n_1s[Nn_1s]; 
  int Nv_1s_04; 
  int v_1s_04[Nv_1s_04]; 
  int Nv_1s_04_z1; 
  int v_1s_04_z1[Nv_1s_04_z1];
  int Nv_1s_08; 
  int v_1s_08[Nv_1s_08]; 
  int Nv_1s_08_z1; 
  int v_1s_08_z1[Nv_1s_08_z1];
  int Nv_1s_10; 
  int Nv_1s_10_z1; 
  int v_1s_10_z1[Nv_1s_10_z1]; 
  int v_1s_10[Nv_1s_10]; 
  int Nvn04; 
  int Nns04; 
  int Nnn04; 
  int Nvs04; 
  int Nn04_h;
  int vn04[Nvn04]; 
  int ns04[Nns04]; 
  int nn04[Nnn04]; 
  int vs04[Nvs04]; 
  int n04_h[Nn04_h];
  int Nvn06; 
  int Nns06; 
  int Nnn06; 
  int Nvs06; 
  int Nn06_h;
  int vn06[Nvn06]; 
  int ns06[Nns06]; 
  int nn06[Nnn06]; 
  int vs06[Nvs06]; 
  int n06_h[Nn06_h]; 
  int Nvn08; 
  int Nns08; 
  int Nnn08; 
  int Nvs08; 
  int Nn08_h;
  int vn08[Nvn08]; 
  int ns08[Nns08]; 
  int nn08[Nnn08]; 
  int vs08[Nvs08]; 
  int n08_h[Nn08_h];
  int Nhh_rest; 
  int hh_rest[Nhh_rest]; 
  int Nindex1_v_1s_04; 
  int Nindex2_v_1s_04; 
  int Nindex3_v_1s_04; 
  int Nindex4_v_1s_04; 
  int Nindex1_v_1s_10; 
  int Nindex2_v_1s_10; 
  int index1_v_1s_04[Nindex1_v_1s_04]; 
  int index2_v_1s_04[Nindex2_v_1s_04];
  int index3_v_1s_04[Nindex3_v_1s_04];
  int index4_v_1s_04[Nindex4_v_1s_04];
  matrix[Nv_1s_04,3] bound_v_1s_04;
  int index1_v_1s_10[Nindex1_v_1s_10]; 
  int index2_v_1s_10[Nindex2_v_1s_10];
  matrix[Nv_1s_10,2] bound_v_1s_10;
  int Nrepeat; 
  int repeat_1[Nrepeat]; 
  int repeat_2[Nrepeat]; 
  int Nnn; 
  int nn[Nnn]; 
}

transformed data {
  real Ttheta; 
  int n; # number of nodes for Gaussian quadrature 
  real data_vs04[Nvs04,2]; 
  real data_vs08[Nvs08, 2];
  real data_ns04[Nns04, 2];
  real data_ns08[Nns08, 2]; 
  real data_vn04[Nvn04, 2];
  real data_vn08[Nvn08, 2];
  real data_nn04[Nnn04, 2];
  real data_nn08[Nnn08, 2];
  real data_08[1];
  real data_04[3];

  matrix[Nhh,2] Premium_mat_vn;  
  matrix[Nhh,2] Premium_mat_vs; 
  matrix[Nhh,2] Premium_mat_nn;
  matrix[Nhh,2] Premium_mat_ns; 
  matrix[100, 2] xw; 

  vector[N_zeta_observed] actual_p; 
  vector[N_zeta_observed] actual_y;
  vector[Ndata] ACTUAL_P;
  vector[Ndata] ACTUAL_Y;   
  real ftol; 
  real xtol; 
  int steptol; 
  real init; 
  real INIT;  
  vector[X_ind_wo_dim] beta_theta;
  vector[X_hh_wo_dim] beta_omega;
  vector[X_ind_wo_dim] beta_gamma;
  vector[X_ind_wo_dim] beta_zeta1;
  vector[X_ind_wo_dim] beta_zeta2; 
  vector[X_ind_wo_dim] beta_lambda;
  vector[X_hh_wo_dim] beta_r;
  real log_sthetabar;
  real log_nu;
  real log_sgamma;
  real log_somega;
  real log_sr; 
  real beta_06_1;
  real beta_04_1;
  real beta_10_1;
  real beta_12_1;
  real beta_06_2;
  real beta_04_2;
  real beta_10_2;
  real beta_12_2;  

  vector[Ndata] mu_zeta1;
  vector[Ndata] mu_zeta2; 
  vector[Ndata] prob0; 
  vector[Ndata] prob1; 
  vector[Ndata] mu_lambda; 
  vector[Ndata] mu_theta;
  vector[Nhh] mu_r; 
  real STHETABAR;
  real NU;
  real SGAMMA;
  real SOMEGA;
  real SR; 
  vector[Ndata] param1_4; 
  vector[Ndata] param1_5; 
  vector[Ndata] param1_6; 

  beta_theta = rep_vector(0, X_ind_wo_dim); 
  beta_theta[1] = -4;
  beta_omega = rep_vector(0, X_hh_wo_dim); 
  beta_omega[1] = -1; 
  beta_gamma = rep_vector(0, X_ind_wo_dim); 
  beta_gamma[1] = -1; 
  beta_zeta1 = rep_vector(0.5, X_ind_wo_dim); 
  beta_zeta2 = rep_vector(0.5, X_ind_wo_dim); 
  beta_lambda = rep_vector(0, X_ind_wo_dim); 
  beta_lambda[1] = 1; 
  beta_r = rep_vector(0, X_hh_wo_dim); 
  beta_r[1] =  -1; 
  log_sr = 0; 
  log_sthetabar = 2; 
  log_nu = 0; 
  log_sgamma = 0; 
  log_somega = 0; 
  beta_06_1 = 0; 
  beta_04_1 = 0; 
  beta_10_1 = 0; 
  beta_12_1 = 0; 
  beta_06_2 = 0; 
  beta_04_2 = 0; 
  beta_10_2 = 0; 
  beta_12_2 = 0; 

 

  ftol = 1e-6; 
  xtol = 1e-5; 
  steptol = 20;
  init = -15.0;  
  INIT = 0.0; 



  n = 100; 
  Ttheta = 10; 
  // Calculate matrix of premium 
    Premium_mat_vn = rep_matrix(0, Nhh,2); 
    Premium_mat_vs = rep_matrix(0, Nhh ,2); 
    Premium_mat_ns = rep_matrix(0,  Nhh ,2); 
    Premium_mat_nn = rep_matrix(0,  Nhh ,2); 
    for (hh in 1:Nhh) {
      for (j in 1:2) {
         Premium_mat_vn[hh,j] =  Premium_fun(N_vol[hh] - (j -1) , Year[hh], Baseprice[hh], HHtype[hh], N_vol[hh] + N_noins[hh] + N_std_wo_ins[hh])
                                  + N_std_w_ins[hh] * Baseprice_s[hh]; 
      }
      for (j in 1:2) {
          Premium_mat_vs[hh,j] =  (N_std_w_ins[hh] - (j - 1))* Baseprice_s[hh] 
                                  + Premium_fun(N_vol[hh], Year[hh], Baseprice[hh], HHtype[hh], N_vol[hh] + N_noins[hh] + N_std_wo_ins[hh]); 
      }
      for (j in 1:2) {
          Premium_mat_nn[hh,j] = Premium_fun(N_vol[hh] + (j -1) , Year[hh], Baseprice[hh], HHtype[hh], N_vol[hh] + N_noins[hh] + N_std_wo_ins[hh]) 
                                  + (N_std_w_ins[hh]* Baseprice_s[hh]); 
      }
      for (j in 1:2) {
          Premium_mat_ns[hh,j] =  (N_std_w_ins[hh] + (j - 1))* Baseprice_s[hh] 
                                  + Premium_fun(N_vol[hh], Year[hh], Baseprice[hh], HHtype[hh], N_vol[hh] + N_noins[hh] + N_std_wo_ins[hh]); 
      }
    }

    # xw = gauss_legendre(n); 

    for (i in 1:Nvs04) {
      data_vs04[i,1] = Yhh[HHid[vs04[i]]] - Premium_mat_vs[HHid[vs04[i]],1]; // y - pi 
      data_vs04[i,2] = Yhh[HHid[vs04[i]]] - Premium_mat_vs[HHid[vs04[i]],2]; // y       
    }   
    data_04[1] = 7500.0/unit_inc; 
    data_04[2] = 0.2;
    data_04[3] = 0;  
    

    for (i in 1:(Nvs08)) {
      data_vs08[i,1] = Yhh[HHid[vs08[i]]] - Premium_mat_vs[HHid[vs08[i]],1]; // y - pi 
      data_vs08[i,2] = Yhh[HHid[vs08[i]]] - Premium_mat_vs[HHid[vs08[i]],2]; // y  
    }
    data_08[1] = 0.2;  

    for (i in 1:Nvn04) {
      data_vn04[i,1] = Yhh[HHid[vn04[i]]] - Premium_mat_vn[HHid[vn04[i]],1]; // y - pi 
      data_vn04[i,2] = Yhh[HHid[vn04[i]]] - Premium_mat_vn[HHid[vn04[i]],2]; // y       
    } 
    for (i in 1:(Nvn08)) {
      data_vn08[i,1] = Yhh[HHid[vn08[i]]] - Premium_mat_vn[HHid[vn08[i]],1]; // y - pi 
      data_vn08[i,2] = Yhh[HHid[vn08[i]]] - Premium_mat_vn[HHid[vn08[i]],2]; // y  
    }

    for (i in 1:Nns04) {
      data_ns04[i,1] = Yhh[HHid[ns04[i]]] - Premium_mat_ns[HHid[ns04[i]],2]; // y - pi 
      data_ns04[i,2] = Yhh[HHid[ns04[i]]] - Premium_mat_ns[HHid[ns04[i]],1]; // y       
    } 
    for (i in 1:(Nns08)) {
      data_ns08[i,1] = Yhh[HHid[ns08[i]]] - Premium_mat_ns[HHid[ns08[i]],2]; // y - pi 
      data_ns08[i,2] = Yhh[HHid[ns08[i]]] - Premium_mat_ns[HHid[ns08[i]],1]; // y  
    }
    for (i in 1:Nnn04) {
      data_nn04[i,1] = Yhh[HHid[nn04[i]]] - Premium_mat_nn[HHid[nn04[i]],2]; // y - pi 
      data_nn04[i,2] = Yhh[HHid[nn04[i]]] - Premium_mat_nn[HHid[nn04[i]],1]; // y       
    } 
    for (i in 1:(Nnn08)) {
      data_nn08[i,1] = Yhh[HHid[nn08[i]]] - Premium_mat_nn[HHid[nn08[i]],2]; // y - pi 
      data_nn08[i,2] = Yhh[HHid[nn08[i]]] - Premium_mat_nn[HHid[nn08[i]],1]; // y  
    }

  // Calculate actual price and income 
    for (i in 1:N_zeta_observed) {
      if (eff_coins[zeta_observed_index[i]] == 1) {
        actual_y[i] = Income[HHid[zeta_observed_index[i]]] - Premium_mat_vs[HHid[zeta_observed_index[i]],1] 
                    - Premium_mat_vn[HHid[zeta_observed_index[i]],1] + 7000.0 * inv(unit_inc); 
      } 
      else {  
        actual_y[i] = Income[HHid[zeta_observed_index[i]]] - Premium_mat_vs[HHid[zeta_observed_index[i]],1] 
                    - Premium_mat_vn[HHid[zeta_observed_index[i]],1]; 
      }
    }
    actual_p = (1 - zeta_observed[zeta_observed_index] ) + zeta_observed[zeta_observed_index] .* eff_coins[zeta_observed_index];  
    actual_p = log(1 + actual_p); 
    actual_y = log(actual_y); 
    ACTUAL_Y = rep_vector(0, Ndata);
    ACTUAL_P = rep_vector(1, Ndata);
    ACTUAL_Y[zeta_observed_index] = actual_y; 
    ACTUAL_P[zeta_observed_index] = actual_p; 

    SR = exp(log_sr); 
    STHETABAR = exp(log_sthetabar); 
    NU = exp(log_nu); 
    SGAMMA = exp(log_sgamma); 
    SOMEGA = exp(log_somega); 

    mu_lambda = (X_ind_wo * beta_lambda)[IVid]; 

    mu_zeta1 = (X_ind_wo * beta_zeta1)[IVid] + beta_04_1 * year04_r + beta_06_1 * year06_r + beta_10_1 * year10_r 
      + beta_12_1 * year12_r; 
    mu_zeta2 = (X_ind_wo * beta_zeta2)[IVid] + beta_04_2 * year04_r + beta_06_2 * year06_r + beta_10_2 * year10_r 
      + beta_12_2 * year12_r; 
    prob0 = exp(mu_zeta1) ./ (1 + exp(mu_zeta1) + exp(mu_zeta2)); 
    prob1 = exp(mu_zeta2) ./ (1 + exp(mu_zeta2) + exp(mu_zeta1));  
    mu_theta = (X_ind_wo * beta_theta)[IVid];


    mu_r = X_hh_wo * beta_r; 

    for (i in 1:Nnn04) { 
        param1_4[nn04[i]] = inv(1 + exp(mu_lambda[nn04[i]])); 
        param1_5[nn04[i]] = prob0[nn04[i]]; 
        param1_6[nn04[i]] = 1 - prob0[nn04[i]]; 
      }

    for (i in 1:Nnn08) { 
        param1_4[nn08[i]] = inv(1 + exp(mu_lambda[nn08[i]])); 
        param1_5[nn08[i]] = prob0[nn08[i]]; 
        param1_6[nn08[i]] = 1 - prob0[nn08[i]]; 
      } 

    for (i in 1:Nns04) { 
        param1_4[ns04[i]] = inv(1 + exp(mu_lambda[ns04[i]])); 
        param1_5[ns04[i]] = prob0[ns04[i]]; 
        param1_6[ns04[i]] = 1 - prob0[ns04[i]]; 
      }
     
    for (i in 1:Nns08) { 
        param1_4[ns08[i]] = inv(1 + exp(mu_lambda[ns08[i]])); 
        param1_5[ns08[i]] = prob0[ns08[i]]; 
        param1_6[ns08[i]] = 1 - prob0[ns08[i]];   
      } 
    for (i in 1:Nvn04) { 
        param1_4[vn04[i]] = inv(1 + exp(mu_lambda[vn04[i]])); 
        param1_5[vn04[i]] = prob0[vn04[i]]; 
        param1_6[vn04[i]] = 1 - prob0[vn04[i]]; 
      }

    for (i in 1:Nvn08) { 
        param1_4[vn08[i]] = inv(1 + exp(mu_lambda[vn08[i]])); 
        param1_5[vn08[i]] = prob0[vn08[i]]; 
        param1_6[vn08[i]] = 1 - prob0[vn08[i]]; 
      }

    for (i in 1:Nvs04) { 
        param1_4[vs04[i]] = inv(1 + exp(mu_lambda[vs04[i]])); 
        param1_5[vs04[i]] = prob0[vs04[i]]; 
        param1_6[vs04[i]] = 1 - prob0[vs04[i]]; 
      }
     
    for (i in 1:Nvs08) { 
        param1_4[vs08[i]] = inv(1 + exp(mu_lambda[vs08[i]])); 
        param1_5[vs08[i]] = prob0[vs08[i]]; 
        param1_6[vs08[i]] = 1 - prob0[vs08[i]];   
      }
}

parameters {
  vector<upper = 0>[Nnn04] theta_nn04;
  vector<upper = 0>[Nnn08] theta_nn08;
  vector<upper = 0>[Nns04] theta_ns04;
  vector<upper = 0>[Nns08] theta_ns08;
  
  vector<upper = 0>[Nvn04] theta_vn04;
  
  vector<upper = 0>[Nvs04] theta_vs04;
  
  vector<upper = 0>[Nvn08] theta_vn08;
  
  vector<upper = 0>[Nvs08] theta_vs08;

  vector<lower = 0, upper = 2>[Nhh] R;
  vector<lower = 0, upper = 2>[Nind] GAMMA_;
  vector<lower = 0, upper = 2>[Nhh] OMEGA;


}

transformed parameters {

}

model {
  vector[Ndata] THETA;
  vector[Ndata] GAMMA;
  GAMMA = GAMMA_[IVid]; 

  
  // Update value of THETA 
    THETA = mu_theta; 

    for (i in 1:Nnn04) {
        if (data_nn04[i,1] <= data_nn04[i,2]) {
          THETA[nn04[i]] = solve_eu(OMEGA[HHid[nn04[i]]], GAMMA[nn04[i]], R[HHid[nn04[i]]], param1_4[nn04[i]], param1_5[nn04[i]], param1_6[nn04[i]], NU, data_nn04[i,:], data_04, init, INIT, ftol, xtol, steptol) + theta_nn04[i];
        }
        else {
          THETA[nn04[i]] = theta_nn04[i];
        }
      }

    for (i in 1:Nnn08) {
        if (data_nn08[i,1] < data_nn08[i,2]) {
          THETA[nn08[i]] = solve_eu(OMEGA[HHid[nn08[i]]], GAMMA[nn08[i]], R[HHid[nn08[i]]], param1_4[nn08[i]], param1_5[nn08[i]], param1_6[nn08[i]], NU, data_nn08[i,:], data_08, init, INIT, ftol, xtol, steptol) + theta_nn08[i];
        }
        else {
          THETA[nn08[i]] = theta_nn08[i];
        }
      } 

    for (i in 1:Nns04) {
        if (data_ns04[i,1] < data_ns04[i,2]) {
          THETA[ns04[i]] = solve_eu(OMEGA[HHid[ns04[i]]], GAMMA[ns04[i]], R[HHid[ns04[i]]], param1_4[ns04[i]], param1_5[ns04[i]], param1_6[ns04[i]], NU, data_ns04[i,:], data_04, init, INIT, ftol, xtol, steptol) + theta_ns04[i];
        }
        else {
          THETA[ns04[i]] = theta_ns04[i];
        }
      }
     
    for (i in 1:Nns08) {
        if (data_ns08[i,1] < data_ns08[i,2]) {
          THETA[ns08[i]] = solve_eu(OMEGA[HHid[ns08[i]]], GAMMA[ns08[i]], R[HHid[ns08[i]]], param1_4[ns08[i]], param1_5[ns08[i]], param1_6[ns08[i]], NU, data_ns08[i,:], data_08, init, INIT, ftol, xtol, steptol) + theta_ns08[i];
        }
        else {
          THETA[ns08[i]] = theta_ns08[i];
        }
      } 

    for (i in 1:Nvn04) {
        if (data_vn04[i,1] <= data_vn04[i,2]) {
          THETA[vn04[i]] = solve_eu(OMEGA[HHid[vn04[i]]], GAMMA[vn04[i]], R[HHid[vn04[i]]], param1_4[vn04[i]], param1_5[vn04[i]], param1_6[vn04[i]], NU, data_vn04[i,:], data_04, init, INIT, ftol, xtol, steptol) - theta_vn04[i];
        }
        else {
          THETA[vn04[i]] = theta_vn04[i]; 
        }
      }

    for (i in 1:Nvn08) {
        if (data_vn08[i,1] < data_vn08[i,2]) {
          THETA[vn08[i]] = solve_eu(OMEGA[HHid[vn08[i]]], GAMMA[vn08[i]], R[HHid[vn08[i]]], param1_4[vn08[i]], param1_5[vn08[i]], param1_6[vn08[i]], NU, data_vn08[i,:], data_08, init, INIT, ftol, xtol, steptol) - theta_vn08[i];
        }
        else {
          THETA[vn08[i]] = theta_vn08[i];
        }
      }

    for (i in 1:Nvs04) {
        if (data_vs04[i,1] < data_vs04[i,2]) {
          THETA[vs04[i]] = solve_eu(OMEGA[HHid[vs04[i]]], GAMMA[vs04[i]], R[HHid[vs04[i]]], param1_4[vs04[i]], param1_5[vs04[i]], param1_6[vs04[i]], NU, data_vs04[i,:], data_04, init, INIT, ftol, xtol, steptol) - theta_vs04[i];
        }
        else {
          THETA[vs04[i]] = theta_vs04[i];
        }
      }
     
    for (i in 1:Nvs08) {
        if (data_vs08[i,1] < data_vs08[i,2]) {
          THETA[vs08[i]] = solve_eu(OMEGA[HHid[vs08[i]]], GAMMA[vs08[i]], R[HHid[vs08[i]]], param1_4[vs08[i]], param1_5[vs08[i]], param1_6[vs08[i]], NU, data_vs08[i,:], data_08, init, INIT, ftol, xtol, steptol) - theta_vs08[i];
        }
        else {
          THETA[vs08[i]] = theta_vs08[i];
        }
      }
  // Update the prior of normalized values 
    GAMMA_ ~ normal((X_ind_wo * beta_gamma),SGAMMA) ;
    OMEGA ~ normal((X_hh_wo * beta_omega),SOMEGA) ;
    R ~ normal((X_hh_wo * beta_r),SR) ; 
    THETA[nn04] ~ normal(mu_theta[nn04],STHETABAR);
    THETA[nn08] ~ normal(mu_theta[nn08],STHETABAR);
    THETA[ns08] ~ normal(mu_theta[ns08],STHETABAR);
    THETA[ns04] ~ normal(mu_theta[ns04],STHETABAR);
    THETA[vn04] ~ normal(mu_theta[vn04],STHETABAR);
    THETA[vs04] ~ normal(mu_theta[vs04],STHETABAR);
    
    THETA[vn08] ~ normal(mu_theta[vn08],STHETABAR);
    THETA[vs08] ~ normal(mu_theta[vs08],STHETABAR);
    
    
    {
      vector[Nnn] logtheta;
      logtheta = log(M_expense[nn]) - (OMEGA[HHid[nn]]) .* log(Income[nn]) 
        + (GAMMA[nn]) * log(2);

      target += normal_lpdf(logtheta | THETA[nn], STHETABAR);
    }

    if (Nv_1s_08 > 0) {
      for (i in 1:Nv_1s_08){
        real temp; 
        real logtheta; 
        int tempindex; 
        tempindex = v_1s_08[i];
        logtheta = log(tot_cost[tempindex]) 
          - (OMEGA[HHid[tempindex]]) * ACTUAL_Y[tempindex] 
          + (GAMMA[tempindex]) * ACTUAL_P[tempindex];

        target += normal_lpdf(logtheta | THETA[tempindex], STHETABAR); 
      }
    }
    
}


















