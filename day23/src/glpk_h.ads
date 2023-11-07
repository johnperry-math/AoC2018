pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with stddef_h;

package glpk_h is

   GLP_MAJOR_VERSION : constant := 5;  --  /usr/include/glpk.h:33
   GLP_MINOR_VERSION : constant := 0;  --  /usr/include/glpk.h:34

   GLP_MIN : constant := 1;  --  /usr/include/glpk.h:40
   GLP_MAX : constant := 2;  --  /usr/include/glpk.h:41

   GLP_CV : constant := 1;  --  /usr/include/glpk.h:44
   GLP_IV : constant := 2;  --  /usr/include/glpk.h:45
   GLP_BV : constant := 3;  --  /usr/include/glpk.h:46

   GLP_FR : constant := 1;  --  /usr/include/glpk.h:49
   GLP_LO : constant := 2;  --  /usr/include/glpk.h:50
   GLP_UP : constant := 3;  --  /usr/include/glpk.h:51
   GLP_DB : constant := 4;  --  /usr/include/glpk.h:52
   GLP_FX : constant := 5;  --  /usr/include/glpk.h:53

   GLP_BS : constant := 1;  --  /usr/include/glpk.h:56
   GLP_NL : constant := 2;  --  /usr/include/glpk.h:57
   GLP_NU : constant := 3;  --  /usr/include/glpk.h:58
   GLP_NF : constant := 4;  --  /usr/include/glpk.h:59
   GLP_NS : constant := 5;  --  /usr/include/glpk.h:60

   GLP_SF_GM : constant := 16#01#;  --  /usr/include/glpk.h:63
   GLP_SF_EQ : constant := 16#10#;  --  /usr/include/glpk.h:64
   GLP_SF_2N : constant := 16#20#;  --  /usr/include/glpk.h:65
   GLP_SF_SKIP : constant := 16#40#;  --  /usr/include/glpk.h:66
   GLP_SF_AUTO : constant := 16#80#;  --  /usr/include/glpk.h:67

   GLP_SOL : constant := 1;  --  /usr/include/glpk.h:70
   GLP_IPT : constant := 2;  --  /usr/include/glpk.h:71
   GLP_MIP : constant := 3;  --  /usr/include/glpk.h:72

   GLP_UNDEF : constant := 1;  --  /usr/include/glpk.h:75
   GLP_FEAS : constant := 2;  --  /usr/include/glpk.h:76
   GLP_INFEAS : constant := 3;  --  /usr/include/glpk.h:77
   GLP_NOFEAS : constant := 4;  --  /usr/include/glpk.h:78
   GLP_OPT : constant := 5;  --  /usr/include/glpk.h:79
   GLP_UNBND : constant := 6;  --  /usr/include/glpk.h:80

   GLP_BF_LUF : constant := 16#00#;  --  /usr/include/glpk.h:87
   GLP_BF_BTF : constant := 16#10#;  --  /usr/include/glpk.h:88

   GLP_BF_FT : constant := 16#01#;  --  /usr/include/glpk.h:90
   GLP_BF_BG : constant := 16#02#;  --  /usr/include/glpk.h:91
   GLP_BF_GR : constant := 16#03#;  --  /usr/include/glpk.h:92

   GLP_MSG_OFF : constant := 0;  --  /usr/include/glpk.h:109
   GLP_MSG_ERR : constant := 1;  --  /usr/include/glpk.h:110
   GLP_MSG_ON : constant := 2;  --  /usr/include/glpk.h:111
   GLP_MSG_ALL : constant := 3;  --  /usr/include/glpk.h:112
   GLP_MSG_DBG : constant := 4;  --  /usr/include/glpk.h:113

   GLP_PRIMAL : constant := 1;  --  /usr/include/glpk.h:115
   GLP_DUALP : constant := 2;  --  /usr/include/glpk.h:116
   GLP_DUAL : constant := 3;  --  /usr/include/glpk.h:117

   GLP_PT_STD : constant := 16#11#;  --  /usr/include/glpk.h:119
   GLP_PT_PSE : constant := 16#22#;  --  /usr/include/glpk.h:120

   GLP_RT_STD : constant := 16#11#;  --  /usr/include/glpk.h:122
   GLP_RT_HAR : constant := 16#22#;  --  /usr/include/glpk.h:123

   GLP_RT_FLIP : constant := 16#33#;  --  /usr/include/glpk.h:125

   GLP_USE_AT : constant := 1;  --  /usr/include/glpk.h:141
   GLP_USE_NT : constant := 2;  --  /usr/include/glpk.h:142

   GLP_ORD_NONE : constant := 0;  --  /usr/include/glpk.h:151
   GLP_ORD_QMD : constant := 1;  --  /usr/include/glpk.h:152
   GLP_ORD_AMD : constant := 2;  --  /usr/include/glpk.h:153
   GLP_ORD_SYMAMD : constant := 3;  --  /usr/include/glpk.h:154

   GLP_BR_FFV : constant := 1;  --  /usr/include/glpk.h:165
   GLP_BR_LFV : constant := 2;  --  /usr/include/glpk.h:166
   GLP_BR_MFV : constant := 3;  --  /usr/include/glpk.h:167
   GLP_BR_DTH : constant := 4;  --  /usr/include/glpk.h:168
   GLP_BR_PCH : constant := 5;  --  /usr/include/glpk.h:169

   GLP_BT_DFS : constant := 1;  --  /usr/include/glpk.h:171
   GLP_BT_BFS : constant := 2;  --  /usr/include/glpk.h:172
   GLP_BT_BLB : constant := 3;  --  /usr/include/glpk.h:173
   GLP_BT_BPH : constant := 4;  --  /usr/include/glpk.h:174

   GLP_PP_NONE : constant := 0;  --  /usr/include/glpk.h:185
   GLP_PP_ROOT : constant := 1;  --  /usr/include/glpk.h:186
   GLP_PP_ALL : constant := 2;  --  /usr/include/glpk.h:187

   GLP_RF_REG : constant := 0;  --  /usr/include/glpk.h:216
   GLP_RF_LAZY : constant := 1;  --  /usr/include/glpk.h:217
   GLP_RF_CUT : constant := 2;  --  /usr/include/glpk.h:218

   GLP_RF_GMI : constant := 1;  --  /usr/include/glpk.h:221
   GLP_RF_MIR : constant := 2;  --  /usr/include/glpk.h:222
   GLP_RF_COV : constant := 3;  --  /usr/include/glpk.h:223
   GLP_RF_CLQ : constant := 4;  --  /usr/include/glpk.h:224

   GLP_ON : constant := 1;  --  /usr/include/glpk.h:230
   GLP_OFF : constant := 0;  --  /usr/include/glpk.h:231

   GLP_IROWGEN : constant := 16#01#;  --  /usr/include/glpk.h:234
   GLP_IBINGO : constant := 16#02#;  --  /usr/include/glpk.h:235
   GLP_IHEUR : constant := 16#03#;  --  /usr/include/glpk.h:236
   GLP_ICUTGEN : constant := 16#04#;  --  /usr/include/glpk.h:237
   GLP_IBRANCH : constant := 16#05#;  --  /usr/include/glpk.h:238
   GLP_ISELECT : constant := 16#06#;  --  /usr/include/glpk.h:239
   GLP_IPREPRO : constant := 16#07#;  --  /usr/include/glpk.h:240

   GLP_NO_BRNCH : constant := 0;  --  /usr/include/glpk.h:243
   GLP_DN_BRNCH : constant := 1;  --  /usr/include/glpk.h:244
   GLP_UP_BRNCH : constant := 2;  --  /usr/include/glpk.h:245

   GLP_EBADB : constant := 16#01#;  --  /usr/include/glpk.h:248
   GLP_ESING : constant := 16#02#;  --  /usr/include/glpk.h:249
   GLP_ECOND : constant := 16#03#;  --  /usr/include/glpk.h:250
   GLP_EBOUND : constant := 16#04#;  --  /usr/include/glpk.h:251
   GLP_EFAIL : constant := 16#05#;  --  /usr/include/glpk.h:252
   GLP_EOBJLL : constant := 16#06#;  --  /usr/include/glpk.h:253
   GLP_EOBJUL : constant := 16#07#;  --  /usr/include/glpk.h:254
   GLP_EITLIM : constant := 16#08#;  --  /usr/include/glpk.h:255
   GLP_ETMLIM : constant := 16#09#;  --  /usr/include/glpk.h:256
   GLP_ENOPFS : constant := 16#0A#;  --  /usr/include/glpk.h:257
   GLP_ENODFS : constant := 16#0B#;  --  /usr/include/glpk.h:258
   GLP_EROOT : constant := 16#0C#;  --  /usr/include/glpk.h:259
   GLP_ESTOP : constant := 16#0D#;  --  /usr/include/glpk.h:260
   GLP_EMIPGAP : constant := 16#0E#;  --  /usr/include/glpk.h:261
   GLP_ENOFEAS : constant := 16#0F#;  --  /usr/include/glpk.h:262
   GLP_ENOCVG : constant := 16#10#;  --  /usr/include/glpk.h:263
   GLP_EINSTAB : constant := 16#11#;  --  /usr/include/glpk.h:264
   GLP_EDATA : constant := 16#12#;  --  /usr/include/glpk.h:265
   GLP_ERANGE : constant := 16#13#;  --  /usr/include/glpk.h:266

   GLP_KKT_PE : constant := 1;  --  /usr/include/glpk.h:269
   GLP_KKT_PB : constant := 2;  --  /usr/include/glpk.h:270
   GLP_KKT_DE : constant := 3;  --  /usr/include/glpk.h:271
   GLP_KKT_DB : constant := 4;  --  /usr/include/glpk.h:272
   GLP_KKT_CS : constant := 5;  --  /usr/include/glpk.h:273

   GLP_MPS_DECK : constant := 1;  --  /usr/include/glpk.h:276
   GLP_MPS_FILE : constant := 2;  --  /usr/include/glpk.h:277
   --  unsupported macro: glp_error glp_error_(__FILE__, __LINE__)
   --  unsupported macro: glp_assert(expr) ((void)((expr) || (glp_assert_(#expr, __FILE__, __LINE__), 1)))
   --  arg-macro: procedure glp_malloc (size)
   --    glp_alloc(1, size)
   --  arg-macro: procedure glp_calloc (n, size)
   --    glp_alloc(n, size)

   GLP_ASN_MIN : constant := 1;  --  /usr/include/glpk.h:1092
   GLP_ASN_MAX : constant := 2;  --  /usr/include/glpk.h:1093
   GLP_ASN_MMP : constant := 3;  --  /usr/include/glpk.h:1094

  -- glpk.h  
  --**********************************************************************
  --*  This code is part of GLPK (GNU Linear Programming Kit).
  --*  Copyright (C) 2000-2020 Free Software Foundation, Inc.
  --*  Written by Andrew Makhorin <mao@gnu.org>.
  --*
  --*  GLPK is free software: you can redistribute it and/or modify it
  --*  under the terms of the GNU General Public License as published by
  --*  the Free Software Foundation, either version 3 of the License, or
  --*  (at your option) any later version.
  --*
  --*  GLPK is distributed in the hope that it will be useful, but WITHOUT
  --*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  --*  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
  --*  License for more details.
  --*
  --*  You should have received a copy of the GNU General Public License
  --*  along with GLPK. If not, see <http://www.gnu.org/licenses/>.
  --********************************************************************** 

  -- library version numbers:  
   type glp_prob is null record;   -- incomplete struct

  -- LP/MIP problem object  
  -- optimization direction flag:  
  -- kind of structural variable:  
  -- type of auxiliary/structural variable:  
  -- status of auxiliary/structural variable:  
  -- scaling options:  
  -- solution indicator:  
  -- solution status:  
  -- basis factorization control parameters  
  -- (not used)  
  -- factorization type:  
  -- (not used)  
  -- sgf_piv_tol  
  -- sgf_piv_lim  
  -- sgf_suhl  
  -- sgf_eps_tol  
  -- (not used)  
  -- fhvint.nfs_max  
  -- (not used)  
  -- scfint.nn_max  
  -- (not used)  
  -- (reserved)  
   type anon_array856 is array (0 .. 37) of aliased double;
   type glp_bfcp is record
      msg_lev : aliased int;  -- /usr/include/glpk.h:84
      c_type : aliased int;  -- /usr/include/glpk.h:85
      lu_size : aliased int;  -- /usr/include/glpk.h:93
      piv_tol : aliased double;  -- /usr/include/glpk.h:94
      piv_lim : aliased int;  -- /usr/include/glpk.h:95
      suhl : aliased int;  -- /usr/include/glpk.h:96
      eps_tol : aliased double;  -- /usr/include/glpk.h:97
      max_gro : aliased double;  -- /usr/include/glpk.h:98
      nfs_max : aliased int;  -- /usr/include/glpk.h:99
      upd_tol : aliased double;  -- /usr/include/glpk.h:100
      nrs_max : aliased int;  -- /usr/include/glpk.h:101
      rs_size : aliased int;  -- /usr/include/glpk.h:102
      foo_bar : aliased anon_array856;  -- /usr/include/glpk.h:103
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:104

  -- simplex solver control parameters  
  -- message level:  
  -- simplex method option:  
  -- pricing technique:  
  -- ratio test technique:  
  -- primal feasibility tolerance  
  -- dual feasibility tolerance  
  -- pivot tolerance  
  -- lower objective limit  
  -- upper objective limit  
  -- simplex iteration limit  
  -- time limit, ms  
  -- display output frequency, ms  
  -- display output delay, ms  
  -- enable/disable using LP presolver  
  -- exclude fixed non-basic variables  
  -- shift bounds of variables to zero  
  -- option to use A or N:  
  -- (reserved)  
   type anon_array860 is array (0 .. 32) of aliased double;
   type glp_smcp is record
      msg_lev : aliased int;  -- /usr/include/glpk.h:108
      meth : aliased int;  -- /usr/include/glpk.h:114
      pricing : aliased int;  -- /usr/include/glpk.h:118
      r_test : aliased int;  -- /usr/include/glpk.h:121
      tol_bnd : aliased double;  -- /usr/include/glpk.h:127
      tol_dj : aliased double;  -- /usr/include/glpk.h:128
      tol_piv : aliased double;  -- /usr/include/glpk.h:129
      obj_ll : aliased double;  -- /usr/include/glpk.h:130
      obj_ul : aliased double;  -- /usr/include/glpk.h:131
      it_lim : aliased int;  -- /usr/include/glpk.h:132
      tm_lim : aliased int;  -- /usr/include/glpk.h:133
      out_frq : aliased int;  -- /usr/include/glpk.h:134
      out_dly : aliased int;  -- /usr/include/glpk.h:135
      presolve : aliased int;  -- /usr/include/glpk.h:136
      excl : aliased int;  -- /usr/include/glpk.h:138
      shift : aliased int;  -- /usr/include/glpk.h:139
      aorn : aliased int;  -- /usr/include/glpk.h:140
      foo_bar : aliased anon_array860;  -- /usr/include/glpk.h:143
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:145

  -- interior-point solver control parameters  
  -- message level (see glp_smcp)  
  -- ordering algorithm:  
  -- (reserved)  
   type anon_array864 is array (0 .. 47) of aliased double;
   type glp_iptcp is record
      msg_lev : aliased int;  -- /usr/include/glpk.h:149
      ord_alg : aliased int;  -- /usr/include/glpk.h:150
      foo_bar : aliased anon_array864;  -- /usr/include/glpk.h:155
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:156

   type glp_tree is null record;   -- incomplete struct

  -- branch-and-bound tree  
  -- integer optimizer control parameters  
  -- message level (see glp_smcp)  
  -- branching technique:  
  -- backtracking technique:  
  -- mip.tol_int  
  -- mip.tol_obj  
  -- mip.tm_lim (milliseconds)  
  -- mip.out_frq (milliseconds)  
  -- mip.out_dly (milliseconds)  
  -- mip.cb_func  
  -- mip.cb_info  
  -- mip.cb_size  
  -- preprocessing technique:  
  -- relative MIP gap tolerance  
  -- MIR cuts       (GLP_ON/GLP_OFF)  
  -- Gomory's cuts  (GLP_ON/GLP_OFF)  
  -- cover cuts     (GLP_ON/GLP_OFF)  
  -- clique cuts    (GLP_ON/GLP_OFF)  
  -- enable/disable using MIP presolver  
  -- try to binarize integer variables  
  -- feasibility pump heuristic  
  -- proximity search heuristic  
  -- proxy time limit, milliseconds  
  -- simple rounding heuristic  
  -- use existing solution  
  -- filename to save every new solution  
  -- use alien solver  
  -- use long-step dual simplex  
  -- (reserved)  
   type anon_array876 is array (0 .. 22) of aliased double;
   type glp_iocp is record
      msg_lev : aliased int;  -- /usr/include/glpk.h:163
      br_tech : aliased int;  -- /usr/include/glpk.h:164
      bt_tech : aliased int;  -- /usr/include/glpk.h:170
      tol_int : aliased double;  -- /usr/include/glpk.h:175
      tol_obj : aliased double;  -- /usr/include/glpk.h:176
      tm_lim : aliased int;  -- /usr/include/glpk.h:177
      out_frq : aliased int;  -- /usr/include/glpk.h:178
      out_dly : aliased int;  -- /usr/include/glpk.h:179
      cb_func : access procedure (arg1 : access glp_tree; arg2 : System.Address);  -- /usr/include/glpk.h:180
      cb_info : System.Address;  -- /usr/include/glpk.h:182
      cb_size : aliased int;  -- /usr/include/glpk.h:183
      pp_tech : aliased int;  -- /usr/include/glpk.h:184
      mip_gap : aliased double;  -- /usr/include/glpk.h:188
      mir_cuts : aliased int;  -- /usr/include/glpk.h:189
      gmi_cuts : aliased int;  -- /usr/include/glpk.h:190
      cov_cuts : aliased int;  -- /usr/include/glpk.h:191
      clq_cuts : aliased int;  -- /usr/include/glpk.h:192
      presolve : aliased int;  -- /usr/include/glpk.h:193
      binarize : aliased int;  -- /usr/include/glpk.h:194
      fp_heur : aliased int;  -- /usr/include/glpk.h:195
      ps_heur : aliased int;  -- /usr/include/glpk.h:196
      ps_tm_lim : aliased int;  -- /usr/include/glpk.h:197
      sr_heur : aliased int;  -- /usr/include/glpk.h:198
      use_sol : aliased int;  -- /usr/include/glpk.h:200
      save_sol : Interfaces.C.Strings.chars_ptr;  -- /usr/include/glpk.h:201
      alien : aliased int;  -- /usr/include/glpk.h:202
      flip : aliased int;  -- /usr/include/glpk.h:205
      foo_bar : aliased anon_array876;  -- /usr/include/glpk.h:207
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:208

  -- additional row attributes  
  -- subproblem level at which the row was added  
  -- row origin flag:  
  -- row class descriptor:  
  -- (reserved)  
   type anon_array880 is array (0 .. 6) of aliased double;
   type glp_attr is record
      level : aliased int;  -- /usr/include/glpk.h:212
      origin : aliased int;  -- /usr/include/glpk.h:214
      klass : aliased int;  -- /usr/include/glpk.h:219
      foo_bar : aliased anon_array880;  -- /usr/include/glpk.h:225
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:227

  -- enable/disable flag:  
  -- reason codes:  
  -- branch selection indicator:  
  -- return codes:  
  -- condition indicator:  
  -- MPS file format:  
  -- MPS format control parameters  
  -- character code to replace blanks in symbolic names  
  -- objective row name  
  -- zero tolerance for MPS data  
  -- (reserved for use in the future)  
   type anon_array884 is array (0 .. 16) of aliased double;
   type glp_mpscp is record
      blank : aliased int;  -- /usr/include/glpk.h:281
      obj_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/glpk.h:283
      tol_mps : aliased double;  -- /usr/include/glpk.h:285
      foo_bar : aliased anon_array884;  -- /usr/include/glpk.h:287
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:289

  -- CPLEX LP format control parameters  
  -- (reserved for use in the future)  
   type anon_array888 is array (0 .. 19) of aliased double;
   type glp_cpxcp is record
      foo_bar : aliased anon_array888;  -- /usr/include/glpk.h:293
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:295

   type glp_prep is null record;   -- incomplete struct

  -- LP/MIP preprocessor workspace  
   type glp_tran is null record;   -- incomplete struct

  -- MathProg translator workspace  
   function glp_create_prob return access glp_prob  -- /usr/include/glpk.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "glp_create_prob";

  -- create problem object  
   procedure glp_set_prob_name (P : access glp_prob; name : Interfaces.C.Strings.chars_ptr)  -- /usr/include/glpk.h:308
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_prob_name";

  -- assign (change) problem name  
   procedure glp_set_obj_name (P : access glp_prob; name : Interfaces.C.Strings.chars_ptr)  -- /usr/include/glpk.h:311
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_obj_name";

  -- assign (change) objective function name  
   procedure glp_set_obj_dir (P : access glp_prob; dir : int)  -- /usr/include/glpk.h:314
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_obj_dir";

  -- set (change) optimization direction flag  
   function glp_add_rows (P : access glp_prob; nrs : int) return int  -- /usr/include/glpk.h:317
   with Import => True, 
        Convention => C, 
        External_Name => "glp_add_rows";

  -- add new rows to problem object  
   function glp_add_cols (P : access glp_prob; ncs : int) return int  -- /usr/include/glpk.h:320
   with Import => True, 
        Convention => C, 
        External_Name => "glp_add_cols";

  -- add new columns to problem object  
   procedure glp_set_row_name
     (P : access glp_prob;
      i : int;
      name : Interfaces.C.Strings.chars_ptr)  -- /usr/include/glpk.h:323
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_row_name";

  -- assign (change) row name  
   procedure glp_set_col_name
     (P : access glp_prob;
      j : int;
      name : Interfaces.C.Strings.chars_ptr)  -- /usr/include/glpk.h:326
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_col_name";

  -- assign (change) column name  
   procedure glp_set_row_bnds
     (P : access glp_prob;
      i : int;
      c_type : int;
      lb : double;
      ub : double)  -- /usr/include/glpk.h:329
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_row_bnds";

  -- set (change) row bounds  
   procedure glp_set_col_bnds
     (P : access glp_prob;
      j : int;
      c_type : int;
      lb : double;
      ub : double)  -- /usr/include/glpk.h:333
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_col_bnds";

  -- set (change) column bounds  
   procedure glp_set_obj_coef
     (P : access glp_prob;
      j : int;
      coef : double)  -- /usr/include/glpk.h:337
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_obj_coef";

  -- set (change) obj. coefficient or constant term  
   procedure glp_set_mat_row
     (P : access glp_prob;
      i : int;
      len : int;
      ind : access int;
      val : access double)  -- /usr/include/glpk.h:340
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_mat_row";

  -- set (replace) row of the constraint matrix  
   procedure glp_set_mat_col
     (P : access glp_prob;
      j : int;
      len : int;
      ind : access int;
      val : access double)  -- /usr/include/glpk.h:344
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_mat_col";

  -- set (replace) column of the constraint matrix  
   procedure glp_load_matrix
     (P : access glp_prob;
      ne : int;
      ia : access int;
      ja : access int;
      ar : access double)  -- /usr/include/glpk.h:348
   with Import => True, 
        Convention => C, 
        External_Name => "glp_load_matrix";

  -- load (replace) the whole constraint matrix  
   function glp_check_dup
     (m : int;
      n : int;
      ne : int;
      ia : access int;
      ja : access int) return int  -- /usr/include/glpk.h:352
   with Import => True, 
        Convention => C, 
        External_Name => "glp_check_dup";

  -- check for duplicate elements in sparse matrix  
   procedure glp_sort_matrix (P : access glp_prob)  -- /usr/include/glpk.h:355
   with Import => True, 
        Convention => C, 
        External_Name => "glp_sort_matrix";

  -- sort elements of the constraint matrix  
   procedure glp_del_rows
     (P : access glp_prob;
      nrs : int;
      num : access int)  -- /usr/include/glpk.h:358
   with Import => True, 
        Convention => C, 
        External_Name => "glp_del_rows";

  -- delete specified rows from problem object  
   procedure glp_del_cols
     (P : access glp_prob;
      ncs : int;
      num : access int)  -- /usr/include/glpk.h:361
   with Import => True, 
        Convention => C, 
        External_Name => "glp_del_cols";

  -- delete specified columns from problem object  
   procedure glp_copy_prob
     (dest : access glp_prob;
      prob : access glp_prob;
      names : int)  -- /usr/include/glpk.h:364
   with Import => True, 
        Convention => C, 
        External_Name => "glp_copy_prob";

  -- copy problem object content  
   procedure glp_erase_prob (P : access glp_prob)  -- /usr/include/glpk.h:367
   with Import => True, 
        Convention => C, 
        External_Name => "glp_erase_prob";

  -- erase problem object content  
   procedure glp_delete_prob (P : access glp_prob)  -- /usr/include/glpk.h:370
   with Import => True, 
        Convention => C, 
        External_Name => "glp_delete_prob";

  -- delete problem object  
   function glp_get_prob_name (P : access glp_prob) return Interfaces.C.Strings.chars_ptr  -- /usr/include/glpk.h:373
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_prob_name";

  -- retrieve problem name  
   function glp_get_obj_name (P : access glp_prob) return Interfaces.C.Strings.chars_ptr  -- /usr/include/glpk.h:376
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_obj_name";

  -- retrieve objective function name  
   function glp_get_obj_dir (P : access glp_prob) return int  -- /usr/include/glpk.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_obj_dir";

  -- retrieve optimization direction flag  
   function glp_get_num_rows (P : access glp_prob) return int  -- /usr/include/glpk.h:382
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_num_rows";

  -- retrieve number of rows  
   function glp_get_num_cols (P : access glp_prob) return int  -- /usr/include/glpk.h:385
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_num_cols";

  -- retrieve number of columns  
   function glp_get_row_name (P : access glp_prob; i : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/glpk.h:388
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_name";

  -- retrieve row name  
   function glp_get_col_name (P : access glp_prob; j : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/glpk.h:391
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_name";

  -- retrieve column name  
   function glp_get_row_type (P : access glp_prob; i : int) return int  -- /usr/include/glpk.h:394
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_type";

  -- retrieve row type  
   function glp_get_row_lb (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:397
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_lb";

  -- retrieve row lower bound  
   function glp_get_row_ub (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:400
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_ub";

  -- retrieve row upper bound  
   function glp_get_col_type (P : access glp_prob; j : int) return int  -- /usr/include/glpk.h:403
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_type";

  -- retrieve column type  
   function glp_get_col_lb (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:406
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_lb";

  -- retrieve column lower bound  
   function glp_get_col_ub (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:409
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_ub";

  -- retrieve column upper bound  
   function glp_get_obj_coef (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:412
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_obj_coef";

  -- retrieve obj. coefficient or constant term  
   function glp_get_num_nz (P : access glp_prob) return int  -- /usr/include/glpk.h:415
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_num_nz";

  -- retrieve number of constraint coefficients  
   function glp_get_mat_row
     (P : access glp_prob;
      i : int;
      ind : access int;
      val : access double) return int  -- /usr/include/glpk.h:418
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_mat_row";

  -- retrieve row of the constraint matrix  
   function glp_get_mat_col
     (P : access glp_prob;
      j : int;
      ind : access int;
      val : access double) return int  -- /usr/include/glpk.h:421
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_mat_col";

  -- retrieve column of the constraint matrix  
   procedure glp_create_index (P : access glp_prob)  -- /usr/include/glpk.h:424
   with Import => True, 
        Convention => C, 
        External_Name => "glp_create_index";

  -- create the name index  
   function glp_find_row (P : access glp_prob; name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:427
   with Import => True, 
        Convention => C, 
        External_Name => "glp_find_row";

  -- find row by its name  
   function glp_find_col (P : access glp_prob; name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:430
   with Import => True, 
        Convention => C, 
        External_Name => "glp_find_col";

  -- find column by its name  
   procedure glp_delete_index (P : access glp_prob)  -- /usr/include/glpk.h:433
   with Import => True, 
        Convention => C, 
        External_Name => "glp_delete_index";

  -- delete the name index  
   procedure glp_set_rii
     (P : access glp_prob;
      i : int;
      rii : double)  -- /usr/include/glpk.h:436
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_rii";

  -- set (change) row scale factor  
   procedure glp_set_sjj
     (P : access glp_prob;
      j : int;
      sjj : double)  -- /usr/include/glpk.h:439
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_sjj";

  -- set (change) column scale factor  
   function glp_get_rii (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:442
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_rii";

  -- retrieve row scale factor  
   function glp_get_sjj (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:445
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_sjj";

  -- retrieve column scale factor  
   procedure glp_scale_prob (P : access glp_prob; flags : int)  -- /usr/include/glpk.h:448
   with Import => True, 
        Convention => C, 
        External_Name => "glp_scale_prob";

  -- scale problem data  
   procedure glp_unscale_prob (P : access glp_prob)  -- /usr/include/glpk.h:451
   with Import => True, 
        Convention => C, 
        External_Name => "glp_unscale_prob";

  -- unscale problem data  
   procedure glp_set_row_stat
     (P : access glp_prob;
      i : int;
      stat : int)  -- /usr/include/glpk.h:454
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_row_stat";

  -- set (change) row status  
   procedure glp_set_col_stat
     (P : access glp_prob;
      j : int;
      stat : int)  -- /usr/include/glpk.h:457
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_col_stat";

  -- set (change) column status  
   procedure glp_std_basis (P : access glp_prob)  -- /usr/include/glpk.h:460
   with Import => True, 
        Convention => C, 
        External_Name => "glp_std_basis";

  -- construct standard initial LP basis  
   procedure glp_adv_basis (P : access glp_prob; flags : int)  -- /usr/include/glpk.h:463
   with Import => True, 
        Convention => C, 
        External_Name => "glp_adv_basis";

  -- construct advanced initial LP basis  
   procedure glp_cpx_basis (P : access glp_prob)  -- /usr/include/glpk.h:466
   with Import => True, 
        Convention => C, 
        External_Name => "glp_cpx_basis";

  -- construct Bixby's initial LP basis  
   function glp_simplex (P : access glp_prob; parm : access constant glp_smcp) return int  -- /usr/include/glpk.h:469
   with Import => True, 
        Convention => C, 
        External_Name => "glp_simplex";

  -- solve LP problem with the simplex method  
   function glp_exact (P : access glp_prob; parm : access constant glp_smcp) return int  -- /usr/include/glpk.h:472
   with Import => True, 
        Convention => C, 
        External_Name => "glp_exact";

  -- solve LP problem in exact arithmetic  
   procedure glp_init_smcp (parm : access glp_smcp)  -- /usr/include/glpk.h:475
   with Import => True, 
        Convention => C, 
        External_Name => "glp_init_smcp";

  -- initialize simplex method control parameters  
   function glp_get_status (P : access glp_prob) return int  -- /usr/include/glpk.h:478
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_status";

  -- retrieve generic status of basic solution  
   function glp_get_prim_stat (P : access glp_prob) return int  -- /usr/include/glpk.h:481
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_prim_stat";

  -- retrieve status of primal basic solution  
   function glp_get_dual_stat (P : access glp_prob) return int  -- /usr/include/glpk.h:484
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_dual_stat";

  -- retrieve status of dual basic solution  
   function glp_get_obj_val (P : access glp_prob) return double  -- /usr/include/glpk.h:487
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_obj_val";

  -- retrieve objective value (basic solution)  
   function glp_get_row_stat (P : access glp_prob; i : int) return int  -- /usr/include/glpk.h:490
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_stat";

  -- retrieve row status  
   function glp_get_row_prim (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:493
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_prim";

  -- retrieve row primal value (basic solution)  
   function glp_get_row_dual (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:496
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_dual";

  -- retrieve row dual value (basic solution)  
   function glp_get_col_stat (P : access glp_prob; j : int) return int  -- /usr/include/glpk.h:499
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_stat";

  -- retrieve column status  
   function glp_get_col_prim (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:502
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_prim";

  -- retrieve column primal value (basic solution)  
   function glp_get_col_dual (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:505
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_dual";

  -- retrieve column dual value (basic solution)  
   function glp_get_unbnd_ray (P : access glp_prob) return int  -- /usr/include/glpk.h:508
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_unbnd_ray";

  -- determine variable causing unboundedness  
   function glp_get_it_cnt (P : access glp_prob) return int  -- /usr/include/glpk.h:512
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_it_cnt";

  -- get simplex solver iteration count  
   procedure glp_set_it_cnt (P : access glp_prob; it_cnt : int)  -- /usr/include/glpk.h:517
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_it_cnt";

  -- set simplex solver iteration count  
   function glp_interior (P : access glp_prob; parm : access constant glp_iptcp) return int  -- /usr/include/glpk.h:521
   with Import => True, 
        Convention => C, 
        External_Name => "glp_interior";

  -- solve LP problem with the interior-point method  
   procedure glp_init_iptcp (parm : access glp_iptcp)  -- /usr/include/glpk.h:524
   with Import => True, 
        Convention => C, 
        External_Name => "glp_init_iptcp";

  -- initialize interior-point solver control parameters  
   function glp_ipt_status (P : access glp_prob) return int  -- /usr/include/glpk.h:527
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ipt_status";

  -- retrieve status of interior-point solution  
   function glp_ipt_obj_val (P : access glp_prob) return double  -- /usr/include/glpk.h:530
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ipt_obj_val";

  -- retrieve objective value (interior point)  
   function glp_ipt_row_prim (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:533
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ipt_row_prim";

  -- retrieve row primal value (interior point)  
   function glp_ipt_row_dual (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:536
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ipt_row_dual";

  -- retrieve row dual value (interior point)  
   function glp_ipt_col_prim (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:539
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ipt_col_prim";

  -- retrieve column primal value (interior point)  
   function glp_ipt_col_dual (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:542
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ipt_col_dual";

  -- retrieve column dual value (interior point)  
   procedure glp_set_col_kind
     (P : access glp_prob;
      j : int;
      kind : int)  -- /usr/include/glpk.h:545
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_col_kind";

  -- set (change) column kind  
   function glp_get_col_kind (P : access glp_prob; j : int) return int  -- /usr/include/glpk.h:548
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_kind";

  -- retrieve column kind  
   function glp_get_num_int (P : access glp_prob) return int  -- /usr/include/glpk.h:551
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_num_int";

  -- retrieve number of integer columns  
   function glp_get_num_bin (P : access glp_prob) return int  -- /usr/include/glpk.h:554
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_num_bin";

  -- retrieve number of binary columns  
   function glp_intopt (P : access glp_prob; parm : access constant glp_iocp) return int  -- /usr/include/glpk.h:557
   with Import => True, 
        Convention => C, 
        External_Name => "glp_intopt";

  -- solve MIP problem with the branch-and-bound method  
   procedure glp_init_iocp (parm : access glp_iocp)  -- /usr/include/glpk.h:560
   with Import => True, 
        Convention => C, 
        External_Name => "glp_init_iocp";

  -- initialize integer optimizer control parameters  
   function glp_mip_status (P : access glp_prob) return int  -- /usr/include/glpk.h:563
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mip_status";

  -- retrieve status of MIP solution  
   function glp_mip_obj_val (P : access glp_prob) return double  -- /usr/include/glpk.h:566
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mip_obj_val";

  -- retrieve objective value (MIP solution)  
   function glp_mip_row_val (P : access glp_prob; i : int) return double  -- /usr/include/glpk.h:569
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mip_row_val";

  -- retrieve row value (MIP solution)  
   function glp_mip_col_val (P : access glp_prob; j : int) return double  -- /usr/include/glpk.h:572
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mip_col_val";

  -- retrieve column value (MIP solution)  
   procedure glp_check_kkt
     (P : access glp_prob;
      sol : int;
      cond : int;
      ae_max : access double;
      ae_ind : access int;
      re_max : access double;
      re_ind : access int)  -- /usr/include/glpk.h:575
   with Import => True, 
        Convention => C, 
        External_Name => "glp_check_kkt";

  -- check feasibility/optimality conditions  
   function glp_print_sol (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:579
   with Import => True, 
        Convention => C, 
        External_Name => "glp_print_sol";

  -- write basic solution in printable format  
   function glp_read_sol (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:582
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_sol";

  -- read basic solution from text file  
   function glp_write_sol (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:585
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_sol";

  -- write basic solution to text file  
   function glp_print_ranges
     (P : access glp_prob;
      len : int;
      list : access int;
      flags : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:588
   with Import => True, 
        Convention => C, 
        External_Name => "glp_print_ranges";

  -- print sensitivity analysis report  
   function glp_print_ipt (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:592
   with Import => True, 
        Convention => C, 
        External_Name => "glp_print_ipt";

  -- write interior-point solution in printable format  
   function glp_read_ipt (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:595
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_ipt";

  -- read interior-point solution from text file  
   function glp_write_ipt (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:598
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_ipt";

  -- write interior-point solution to text file  
   function glp_print_mip (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:601
   with Import => True, 
        Convention => C, 
        External_Name => "glp_print_mip";

  -- write MIP solution in printable format  
   function glp_read_mip (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:604
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_mip";

  -- read MIP solution from text file  
   function glp_write_mip (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:607
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_mip";

  -- write MIP solution to text file  
   function glp_bf_exists (P : access glp_prob) return int  -- /usr/include/glpk.h:610
   with Import => True, 
        Convention => C, 
        External_Name => "glp_bf_exists";

  -- check if LP basis factorization exists  
   function glp_factorize (P : access glp_prob) return int  -- /usr/include/glpk.h:613
   with Import => True, 
        Convention => C, 
        External_Name => "glp_factorize";

  -- compute LP basis factorization  
   function glp_bf_updated (P : access glp_prob) return int  -- /usr/include/glpk.h:616
   with Import => True, 
        Convention => C, 
        External_Name => "glp_bf_updated";

  -- check if LP basis factorization has been updated  
   procedure glp_get_bfcp (P : access glp_prob; parm : access glp_bfcp)  -- /usr/include/glpk.h:619
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_bfcp";

  -- retrieve LP basis factorization control parameters  
   procedure glp_set_bfcp (P : access glp_prob; parm : access constant glp_bfcp)  -- /usr/include/glpk.h:622
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_bfcp";

  -- change LP basis factorization control parameters  
   function glp_get_bhead (P : access glp_prob; k : int) return int  -- /usr/include/glpk.h:625
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_bhead";

  -- retrieve LP basis header information  
   function glp_get_row_bind (P : access glp_prob; i : int) return int  -- /usr/include/glpk.h:628
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_row_bind";

  -- retrieve row index in the basis header  
   function glp_get_col_bind (P : access glp_prob; j : int) return int  -- /usr/include/glpk.h:631
   with Import => True, 
        Convention => C, 
        External_Name => "glp_get_col_bind";

  -- retrieve column index in the basis header  
   procedure glp_ftran (P : access glp_prob; x : access double)  -- /usr/include/glpk.h:634
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ftran";

  -- perform forward transformation (solve system B*x = b)  
   procedure glp_btran (P : access glp_prob; x : access double)  -- /usr/include/glpk.h:637
   with Import => True, 
        Convention => C, 
        External_Name => "glp_btran";

  -- perform backward transformation (solve system B'*x = b)  
   function glp_warm_up (P : access glp_prob) return int  -- /usr/include/glpk.h:640
   with Import => True, 
        Convention => C, 
        External_Name => "glp_warm_up";

  -- "warm up" LP basis  
   function glp_eval_tab_row
     (P : access glp_prob;
      k : int;
      ind : access int;
      val : access double) return int  -- /usr/include/glpk.h:643
   with Import => True, 
        Convention => C, 
        External_Name => "glp_eval_tab_row";

  -- compute row of the simplex tableau  
   function glp_eval_tab_col
     (P : access glp_prob;
      k : int;
      ind : access int;
      val : access double) return int  -- /usr/include/glpk.h:646
   with Import => True, 
        Convention => C, 
        External_Name => "glp_eval_tab_col";

  -- compute column of the simplex tableau  
   function glp_transform_row
     (P : access glp_prob;
      len : int;
      ind : access int;
      val : access double) return int  -- /usr/include/glpk.h:649
   with Import => True, 
        Convention => C, 
        External_Name => "glp_transform_row";

  -- transform explicitly specified row  
   function glp_transform_col
     (P : access glp_prob;
      len : int;
      ind : access int;
      val : access double) return int  -- /usr/include/glpk.h:652
   with Import => True, 
        Convention => C, 
        External_Name => "glp_transform_col";

  -- transform explicitly specified column  
   function glp_prim_rtest
     (P : access glp_prob;
      len : int;
      ind : access int;
      val : access double;
      dir : int;
      eps : double) return int  -- /usr/include/glpk.h:655
   with Import => True, 
        Convention => C, 
        External_Name => "glp_prim_rtest";

  -- perform primal ratio test  
   function glp_dual_rtest
     (P : access glp_prob;
      len : int;
      ind : access int;
      val : access double;
      dir : int;
      eps : double) return int  -- /usr/include/glpk.h:659
   with Import => True, 
        Convention => C, 
        External_Name => "glp_dual_rtest";

  -- perform dual ratio test  
   procedure glp_analyze_bound
     (P : access glp_prob;
      k : int;
      value1 : access double;
      var1 : access int;
      value2 : access double;
      var2 : access int)  -- /usr/include/glpk.h:663
   with Import => True, 
        Convention => C, 
        External_Name => "glp_analyze_bound";

  -- analyze active bound of non-basic variable  
   procedure glp_analyze_coef
     (P : access glp_prob;
      k : int;
      coef1 : access double;
      var1 : access int;
      value1 : access double;
      coef2 : access double;
      var2 : access int;
      value2 : access double)  -- /usr/include/glpk.h:667
   with Import => True, 
        Convention => C, 
        External_Name => "glp_analyze_coef";

  -- analyze objective coefficient at basic variable  
   function glp_npp_alloc_wksp return access glp_prep  -- /usr/include/glpk.h:672
   with Import => True, 
        Convention => C, 
        External_Name => "glp_npp_alloc_wksp";

  -- allocate the preprocessor workspace  
   procedure glp_npp_load_prob
     (prep : access glp_prep;
      P : access glp_prob;
      sol : int;
      names : int)  -- /usr/include/glpk.h:675
   with Import => True, 
        Convention => C, 
        External_Name => "glp_npp_load_prob";

  -- load original problem instance  
   function glp_npp_preprocess1 (prep : access glp_prep; hard : int) return int  -- /usr/include/glpk.h:679
   with Import => True, 
        Convention => C, 
        External_Name => "glp_npp_preprocess1";

  -- perform basic LP/MIP preprocessing  
   procedure glp_npp_build_prob (prep : access glp_prep; Q : access glp_prob)  -- /usr/include/glpk.h:682
   with Import => True, 
        Convention => C, 
        External_Name => "glp_npp_build_prob";

  -- build resultant problem instance  
   procedure glp_npp_postprocess (prep : access glp_prep; Q : access glp_prob)  -- /usr/include/glpk.h:685
   with Import => True, 
        Convention => C, 
        External_Name => "glp_npp_postprocess";

  -- postprocess solution to resultant problem  
   procedure glp_npp_obtain_sol (prep : access glp_prep; P : access glp_prob)  -- /usr/include/glpk.h:688
   with Import => True, 
        Convention => C, 
        External_Name => "glp_npp_obtain_sol";

  -- obtain solution to original problem  
   procedure glp_npp_free_wksp (prep : access glp_prep)  -- /usr/include/glpk.h:691
   with Import => True, 
        Convention => C, 
        External_Name => "glp_npp_free_wksp";

  -- free the preprocessor workspace  
   function glp_ios_reason (T : access glp_tree) return int  -- /usr/include/glpk.h:695
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_reason";

  -- determine reason for calling the callback routine  
   function glp_ios_get_prob (T : access glp_tree) return access glp_prob  -- /usr/include/glpk.h:698
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_get_prob";

  -- access the problem object  
   procedure glp_ios_tree_size
     (T : access glp_tree;
      a_cnt : access int;
      n_cnt : access int;
      t_cnt : access int)  -- /usr/include/glpk.h:701
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_tree_size";

  -- determine size of the branch-and-bound tree  
   function glp_ios_curr_node (T : access glp_tree) return int  -- /usr/include/glpk.h:705
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_curr_node";

  -- determine current active subproblem  
   function glp_ios_next_node (T : access glp_tree; p : int) return int  -- /usr/include/glpk.h:708
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_next_node";

  -- determine next active subproblem  
   function glp_ios_prev_node (T : access glp_tree; p : int) return int  -- /usr/include/glpk.h:711
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_prev_node";

  -- determine previous active subproblem  
   function glp_ios_up_node (T : access glp_tree; p : int) return int  -- /usr/include/glpk.h:714
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_up_node";

  -- determine parent subproblem  
   function glp_ios_node_level (T : access glp_tree; p : int) return int  -- /usr/include/glpk.h:717
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_node_level";

  -- determine subproblem level  
   function glp_ios_node_bound (T : access glp_tree; p : int) return double  -- /usr/include/glpk.h:720
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_node_bound";

  -- determine subproblem local bound  
   function glp_ios_best_node (T : access glp_tree) return int  -- /usr/include/glpk.h:723
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_best_node";

  -- find active subproblem with best local bound  
   function glp_ios_mip_gap (T : access glp_tree) return double  -- /usr/include/glpk.h:726
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_mip_gap";

  -- compute relative MIP gap  
   function glp_ios_node_data (T : access glp_tree; p : int) return System.Address  -- /usr/include/glpk.h:729
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_node_data";

  -- access subproblem application-specific data  
   procedure glp_ios_row_attr
     (T : access glp_tree;
      i : int;
      attr : access glp_attr)  -- /usr/include/glpk.h:732
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_row_attr";

  -- retrieve additional row attributes  
   function glp_ios_pool_size (T : access glp_tree) return int  -- /usr/include/glpk.h:735
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_pool_size";

  -- determine current size of the cut pool  
   function glp_ios_add_row
     (T : access glp_tree;
      name : Interfaces.C.Strings.chars_ptr;
      klass : int;
      flags : int;
      len : int;
      ind : access int;
      val : access double;
      c_type : int;
      rhs : double) return int  -- /usr/include/glpk.h:738
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_add_row";

  -- add row (constraint) to the cut pool  
   procedure glp_ios_del_row (T : access glp_tree; i : int)  -- /usr/include/glpk.h:743
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_del_row";

  -- remove row (constraint) from the cut pool  
   procedure glp_ios_clear_pool (T : access glp_tree)  -- /usr/include/glpk.h:746
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_clear_pool";

  -- remove all rows (constraints) from the cut pool  
   function glp_ios_can_branch (T : access glp_tree; j : int) return int  -- /usr/include/glpk.h:749
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_can_branch";

  -- check if can branch upon specified variable  
   procedure glp_ios_branch_upon
     (T : access glp_tree;
      j : int;
      sel : int)  -- /usr/include/glpk.h:752
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_branch_upon";

  -- choose variable to branch upon  
   procedure glp_ios_select_node (T : access glp_tree; p : int)  -- /usr/include/glpk.h:755
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_select_node";

  -- select subproblem to continue the search  
   function glp_ios_heur_sol (T : access glp_tree; x : access double) return int  -- /usr/include/glpk.h:758
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_heur_sol";

  -- provide solution found by heuristic  
   procedure glp_ios_terminate (T : access glp_tree)  -- /usr/include/glpk.h:761
   with Import => True, 
        Convention => C, 
        External_Name => "glp_ios_terminate";

  -- terminate the solution process  
  -- generate Gomory's mixed integer cut (core routine)  
  -- generate Gomory's mixed integer cuts  
  -- cover cur generator workspace  
  -- create and initialize cover cut generator  
  -- generate locally valid simple cover cuts  
  -- delete cover cut generator workspace  
  -- MIR cut generator workspace  
  -- create and initialize MIR cut generator  
  -- generate mixed integer rounding (MIR) cuts  
  -- delete MIR cut generator workspace  
  -- conflict graph descriptor  
  -- create and initialize conflict graph  
  -- delete conflict graph descriptor  
  -- generate clique cut from conflict graph  
   procedure glp_init_mpscp (parm : access glp_mpscp)  -- /usr/include/glpk.h:809
   with Import => True, 
        Convention => C, 
        External_Name => "glp_init_mpscp";

  -- initialize MPS format control parameters  
   function glp_read_mps
     (P : access glp_prob;
      fmt : int;
      parm : access constant glp_mpscp;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:812
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_mps";

  -- read problem data in MPS format  
   function glp_write_mps
     (P : access glp_prob;
      fmt : int;
      parm : access constant glp_mpscp;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:816
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_mps";

  -- write problem data in MPS format  
   procedure glp_init_cpxcp (parm : access glp_cpxcp)  -- /usr/include/glpk.h:820
   with Import => True, 
        Convention => C, 
        External_Name => "glp_init_cpxcp";

  -- initialize CPLEX LP format control parameters  
   function glp_read_lp
     (P : access glp_prob;
      parm : access constant glp_cpxcp;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:823
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_lp";

  -- read problem data in CPLEX LP format  
   function glp_write_lp
     (P : access glp_prob;
      parm : access constant glp_cpxcp;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:826
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_lp";

  -- write problem data in CPLEX LP format  
   function glp_read_prob
     (P : access glp_prob;
      flags : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:829
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_prob";

  -- read problem data in GLPK format  
   function glp_write_prob
     (P : access glp_prob;
      flags : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:832
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_prob";

  -- write problem data in GLPK format  
   function glp_mpl_alloc_wksp return access glp_tran  -- /usr/include/glpk.h:835
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_alloc_wksp";

  -- allocate the MathProg translator workspace  
   procedure glp_mpl_init_rand (tran : access glp_tran; seed : int)  -- /usr/include/glpk.h:838
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_init_rand";

  -- initialize pseudo-random number generator  
   function glp_mpl_read_model
     (tran : access glp_tran;
      fname : Interfaces.C.Strings.chars_ptr;
      skip : int) return int  -- /usr/include/glpk.h:841
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_read_model";

  -- read and translate model section  
   function glp_mpl_read_data (tran : access glp_tran; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:844
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_read_data";

  -- read and translate data section  
   function glp_mpl_generate (tran : access glp_tran; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:847
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_generate";

  -- generate the model  
   procedure glp_mpl_build_prob (tran : access glp_tran; prob : access glp_prob)  -- /usr/include/glpk.h:850
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_build_prob";

  -- build LP/MIP problem instance from the model  
   function glp_mpl_postsolve
     (tran : access glp_tran;
      prob : access glp_prob;
      sol : int) return int  -- /usr/include/glpk.h:853
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_postsolve";

  -- postsolve the model  
   procedure glp_mpl_free_wksp (tran : access glp_tran)  -- /usr/include/glpk.h:856
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mpl_free_wksp";

  -- free the MathProg translator workspace  
   function glp_read_cnfsat (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:859
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_cnfsat";

  -- read CNF-SAT problem data in DIMACS format  
   function glp_check_cnfsat (P : access glp_prob) return int  -- /usr/include/glpk.h:862
   with Import => True, 
        Convention => C, 
        External_Name => "glp_check_cnfsat";

  -- check for CNF-SAT problem instance  
   function glp_write_cnfsat (P : access glp_prob; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:865
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_cnfsat";

  -- write CNF-SAT problem data in DIMACS format  
   function glp_minisat1 (P : access glp_prob) return int  -- /usr/include/glpk.h:868
   with Import => True, 
        Convention => C, 
        External_Name => "glp_minisat1";

  -- solve CNF-SAT problem with MiniSat solver  
   function glp_intfeas1
     (P : access glp_prob;
      use_bound : int;
      obj_bound : int) return int  -- /usr/include/glpk.h:871
   with Import => True, 
        Convention => C, 
        External_Name => "glp_intfeas1";

  -- solve integer feasibility problem  
   function glp_init_env return int  -- /usr/include/glpk.h:874
   with Import => True, 
        Convention => C, 
        External_Name => "glp_init_env";

  -- initialize GLPK environment  
   function glp_version return Interfaces.C.Strings.chars_ptr  -- /usr/include/glpk.h:877
   with Import => True, 
        Convention => C, 
        External_Name => "glp_version";

  -- determine library version  
   function glp_config (option : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/glpk.h:880
   with Import => True, 
        Convention => C, 
        External_Name => "glp_config";

  -- determine library configuration  
   function glp_free_env return int  -- /usr/include/glpk.h:883
   with Import => True, 
        Convention => C, 
        External_Name => "glp_free_env";

  -- free GLPK environment  
   procedure glp_puts (s : Interfaces.C.Strings.chars_ptr)  -- /usr/include/glpk.h:886
   with Import => True, 
        Convention => C, 
        External_Name => "glp_puts";

  -- write string on terminal  
   procedure glp_printf (fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      )  -- /usr/include/glpk.h:889
   with Import => True, 
        Convention => C, 
        External_Name => "glp_printf";

  -- write formatted output on terminal  
   procedure glp_vprintf (fmt : Interfaces.C.Strings.chars_ptr; arg : access System.Address)  -- /usr/include/glpk.h:892
   with Import => True, 
        Convention => C, 
        External_Name => "glp_vprintf";

  -- write formatted output on terminal  
   function glp_term_out (flag : int) return int  -- /usr/include/glpk.h:895
   with Import => True, 
        Convention => C, 
        External_Name => "glp_term_out";

  -- enable/disable terminal output  
   procedure glp_term_hook (func : access function (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return int; info : System.Address)  -- /usr/include/glpk.h:898
   with Import => True, 
        Convention => C, 
        External_Name => "glp_term_hook";

  -- install hook to intercept terminal output  
   function glp_open_tee (name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:901
   with Import => True, 
        Convention => C, 
        External_Name => "glp_open_tee";

  -- start copying terminal output to text file  
   function glp_close_tee return int  -- /usr/include/glpk.h:904
   with Import => True, 
        Convention => C, 
        External_Name => "glp_close_tee";

  -- stop copying terminal output to text file  
   type glp_errfunc is access procedure (arg1 : Interfaces.C.Strings.chars_ptr  -- , ...
         )
   with Convention => C;  -- /usr/include/glpk.h:909

   function glp_error_u (file : Interfaces.C.Strings.chars_ptr; line : int) return glp_errfunc  -- /usr/include/glpk.h:913
   with Import => True, 
        Convention => C, 
        External_Name => "glp_error_";

  -- display fatal error message and terminate execution  
   function glp_at_error return int  -- /usr/include/glpk.h:917
   with Import => True, 
        Convention => C, 
        External_Name => "glp_at_error";

  -- check for error state  
   procedure glp_assert_u
     (expr : Interfaces.C.Strings.chars_ptr;
      file : Interfaces.C.Strings.chars_ptr;
      line : int)  -- /usr/include/glpk.h:923
   with Import => True, 
        Convention => C, 
        External_Name => "glp_assert_";

  -- check for logical condition  
   procedure glp_error_hook (func : access procedure (arg1 : System.Address); info : System.Address)  -- /usr/include/glpk.h:926
   with Import => True, 
        Convention => C, 
        External_Name => "glp_error_hook";

  -- install hook to intercept abnormal termination  
  -- allocate memory block (obsolete)  
  -- allocate memory block (obsolete)  
   function glp_alloc (n : int; size : int) return System.Address  -- /usr/include/glpk.h:935
   with Import => True, 
        Convention => C, 
        External_Name => "glp_alloc";

  -- allocate memory block  
   function glp_realloc
     (ptr : System.Address;
      n : int;
      size : int) return System.Address  -- /usr/include/glpk.h:938
   with Import => True, 
        Convention => C, 
        External_Name => "glp_realloc";

  -- reallocate memory block  
   procedure glp_free (ptr : System.Address)  -- /usr/include/glpk.h:941
   with Import => True, 
        Convention => C, 
        External_Name => "glp_free";

  -- free (deallocate) memory block  
   procedure glp_mem_limit (limit : int)  -- /usr/include/glpk.h:944
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mem_limit";

  -- set memory usage limit  
   procedure glp_mem_usage
     (count : access int;
      cpeak : access int;
      total : access stddef_h.size_t;
      tpeak : access stddef_h.size_t)  -- /usr/include/glpk.h:947
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mem_usage";

  -- get memory usage information  
   function glp_time return double  -- /usr/include/glpk.h:951
   with Import => True, 
        Convention => C, 
        External_Name => "glp_time";

  -- determine current universal time  
   function glp_difftime (t1 : double; t0 : double) return double  -- /usr/include/glpk.h:954
   with Import => True, 
        Convention => C, 
        External_Name => "glp_difftime";

  -- compute difference between two time values  
   type glp_graph;
   type glp_vertex;
   type glp_arc;
  -- graph descriptor  
  -- DMP *pool;  
   type glp_graph is record
      pool : System.Address;  -- /usr/include/glpk.h:963
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/glpk.h:965
      nv_max : aliased int;  -- /usr/include/glpk.h:968
      nv : aliased int;  -- /usr/include/glpk.h:970
      na : aliased int;  -- /usr/include/glpk.h:972
      v : System.Address;  -- /usr/include/glpk.h:974
      index : System.Address;  -- /usr/include/glpk.h:976
      v_size : aliased int;  -- /usr/include/glpk.h:979
      a_size : aliased int;  -- /usr/include/glpk.h:981
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:961

  -- memory pool to store graph components  
  -- graph name (1 to 255 chars); NULL means no name is assigned
  --         to the graph  

  -- length of the vertex list (enlarged automatically)  
  -- number of vertices in the graph, 0 <= nv <= nv_max  
  -- number of arcs in the graph, na >= 0  
  -- glp_vertex *v[1+nv_max];  
  -- v[i], 1 <= i <= nv, is a pointer to i-th vertex  
  -- AVL *index;  
  -- vertex index to find vertices by their names; NULL means the
  --         index does not exist  

  -- size of data associated with each vertex (0 to 256 bytes)  
  -- size of data associated with each arc (0 to 256 bytes)  
  -- vertex descriptor  
   type glp_vertex is record
      i : aliased int;  -- /usr/include/glpk.h:987
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/glpk.h:989
      c_entry : System.Address;  -- /usr/include/glpk.h:992
      data : System.Address;  -- /usr/include/glpk.h:996
      temp : System.Address;  -- /usr/include/glpk.h:998
      c_in : access glp_arc;  -- /usr/include/glpk.h:1000
      c_out : access glp_arc;  -- /usr/include/glpk.h:1002
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:985

  -- vertex ordinal number, 1 <= i <= nv  
  -- vertex name (1 to 255 chars); NULL means no name is assigned
  --         to the vertex  

  -- AVLNODE *entry;  
  -- pointer to corresponding entry in the vertex index; NULL means
  --         that either the index does not exist or the vertex has no name
  --         assigned  

  -- pointer to data associated with the vertex  
  -- working pointer  
  -- pointer to the (unordered) list of incoming arcs  
  -- pointer to the (unordered) list of outgoing arcs  
  -- arc descriptor  
   type glp_arc is record
      tail : access glp_vertex;  -- /usr/include/glpk.h:1008
      head : access glp_vertex;  -- /usr/include/glpk.h:1010
      data : System.Address;  -- /usr/include/glpk.h:1012
      temp : System.Address;  -- /usr/include/glpk.h:1014
      t_prev : access glp_arc;  -- /usr/include/glpk.h:1016
      t_next : access glp_arc;  -- /usr/include/glpk.h:1018
      h_prev : access glp_arc;  -- /usr/include/glpk.h:1020
      h_next : access glp_arc;  -- /usr/include/glpk.h:1022
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/glpk.h:1006

  -- pointer to the tail endpoint  
  -- pointer to the head endpoint  
  -- pointer to data associated with the arc  
  -- working pointer  
  -- pointer to previous arc having the same tail endpoint  
  -- pointer to next arc having the same tail endpoint  
  -- pointer to previous arc having the same head endpoint  
  -- pointer to next arc having the same head endpoint  
   function glp_create_graph (v_size : int; a_size : int) return access glp_graph  -- /usr/include/glpk.h:1026
   with Import => True, 
        Convention => C, 
        External_Name => "glp_create_graph";

  -- create graph  
   procedure glp_set_graph_name (G : access glp_graph; name : Interfaces.C.Strings.chars_ptr)  -- /usr/include/glpk.h:1029
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_graph_name";

  -- assign (change) graph name  
   function glp_add_vertices (G : access glp_graph; nadd : int) return int  -- /usr/include/glpk.h:1032
   with Import => True, 
        Convention => C, 
        External_Name => "glp_add_vertices";

  -- add new vertices to graph  
   procedure glp_set_vertex_name
     (G : access glp_graph;
      i : int;
      name : Interfaces.C.Strings.chars_ptr)  -- /usr/include/glpk.h:1035
   with Import => True, 
        Convention => C, 
        External_Name => "glp_set_vertex_name";

  -- assign (change) vertex name  
   function glp_add_arc
     (G : access glp_graph;
      i : int;
      j : int) return access glp_arc  -- /usr/include/glpk.h:1038
   with Import => True, 
        Convention => C, 
        External_Name => "glp_add_arc";

  -- add new arc to graph  
   procedure glp_del_vertices
     (G : access glp_graph;
      ndel : int;
      num : access int)  -- /usr/include/glpk.h:1041
   with Import => True, 
        Convention => C, 
        External_Name => "glp_del_vertices";

  -- delete vertices from graph  
   procedure glp_del_arc (G : access glp_graph; a : access glp_arc)  -- /usr/include/glpk.h:1044
   with Import => True, 
        Convention => C, 
        External_Name => "glp_del_arc";

  -- delete arc from graph  
   procedure glp_erase_graph
     (G : access glp_graph;
      v_size : int;
      a_size : int)  -- /usr/include/glpk.h:1047
   with Import => True, 
        Convention => C, 
        External_Name => "glp_erase_graph";

  -- erase graph content  
   procedure glp_delete_graph (G : access glp_graph)  -- /usr/include/glpk.h:1050
   with Import => True, 
        Convention => C, 
        External_Name => "glp_delete_graph";

  -- delete graph  
   procedure glp_create_v_index (G : access glp_graph)  -- /usr/include/glpk.h:1053
   with Import => True, 
        Convention => C, 
        External_Name => "glp_create_v_index";

  -- create vertex name index  
   function glp_find_vertex (G : access glp_graph; name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1056
   with Import => True, 
        Convention => C, 
        External_Name => "glp_find_vertex";

  -- find vertex by its name  
   procedure glp_delete_v_index (G : access glp_graph)  -- /usr/include/glpk.h:1059
   with Import => True, 
        Convention => C, 
        External_Name => "glp_delete_v_index";

  -- delete vertex name index  
   function glp_read_graph (G : access glp_graph; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1062
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_graph";

  -- read graph from plain text file  
   function glp_write_graph (G : access glp_graph; fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1065
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_graph";

  -- write graph to plain text file  
   procedure glp_mincost_lp
     (P : access glp_prob;
      G : access glp_graph;
      names : int;
      v_rhs : int;
      a_low : int;
      a_cap : int;
      a_cost : int)  -- /usr/include/glpk.h:1068
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mincost_lp";

  -- convert minimum cost flow problem to LP  
   function glp_mincost_okalg
     (G : access glp_graph;
      v_rhs : int;
      a_low : int;
      a_cap : int;
      a_cost : int;
      sol : access double;
      a_x : int;
      v_pi : int) return int  -- /usr/include/glpk.h:1072
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mincost_okalg";

  -- find minimum-cost flow with out-of-kilter algorithm  
   function glp_mincost_relax4
     (G : access glp_graph;
      v_rhs : int;
      a_low : int;
      a_cap : int;
      a_cost : int;
      crash : int;
      sol : access double;
      a_x : int;
      a_rc : int) return int  -- /usr/include/glpk.h:1076
   with Import => True, 
        Convention => C, 
        External_Name => "glp_mincost_relax4";

  -- find minimum-cost flow with Bertsekas-Tseng relaxation method  
   procedure glp_maxflow_lp
     (P : access glp_prob;
      G : access glp_graph;
      names : int;
      s : int;
      t : int;
      a_cap : int)  -- /usr/include/glpk.h:1080
   with Import => True, 
        Convention => C, 
        External_Name => "glp_maxflow_lp";

  -- convert maximum flow problem to LP  
   function glp_maxflow_ffalg
     (G : access glp_graph;
      s : int;
      t : int;
      a_cap : int;
      sol : access double;
      a_x : int;
      v_cut : int) return int  -- /usr/include/glpk.h:1084
   with Import => True, 
        Convention => C, 
        External_Name => "glp_maxflow_ffalg";

  -- find maximal flow with Ford-Fulkerson algorithm  
   function glp_check_asnprob (G : access glp_graph; v_set : int) return int  -- /usr/include/glpk.h:1088
   with Import => True, 
        Convention => C, 
        External_Name => "glp_check_asnprob";

  -- check correctness of assignment problem data  
  -- assignment problem formulation:  
   function glp_asnprob_lp
     (P : access glp_prob;
      form : int;
      G : access glp_graph;
      names : int;
      v_set : int;
      a_cost : int) return int  -- /usr/include/glpk.h:1096
   with Import => True, 
        Convention => C, 
        External_Name => "glp_asnprob_lp";

  -- convert assignment problem to LP  
   function glp_asnprob_okalg
     (form : int;
      G : access glp_graph;
      v_set : int;
      a_cost : int;
      sol : access double;
      a_x : int) return int  -- /usr/include/glpk.h:1100
   with Import => True, 
        Convention => C, 
        External_Name => "glp_asnprob_okalg";

  -- solve assignment problem with out-of-kilter algorithm  
   function glp_asnprob_hall
     (G : access glp_graph;
      v_set : int;
      a_x : int) return int  -- /usr/include/glpk.h:1104
   with Import => True, 
        Convention => C, 
        External_Name => "glp_asnprob_hall";

  -- find bipartite matching of maximum cardinality  
   function glp_cpp
     (G : access glp_graph;
      v_t : int;
      v_es : int;
      v_ls : int) return double  -- /usr/include/glpk.h:1107
   with Import => True, 
        Convention => C, 
        External_Name => "glp_cpp";

  -- solve critical path problem  
   function glp_read_mincost
     (G : access glp_graph;
      v_rhs : int;
      a_low : int;
      a_cap : int;
      a_cost : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1110
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_mincost";

  -- read min-cost flow problem data in DIMACS format  
   function glp_write_mincost
     (G : access glp_graph;
      v_rhs : int;
      a_low : int;
      a_cap : int;
      a_cost : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1114
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_mincost";

  -- write min-cost flow problem data in DIMACS format  
   function glp_read_maxflow
     (G : access glp_graph;
      s : access int;
      t : access int;
      a_cap : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1118
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_maxflow";

  -- read maximum flow problem data in DIMACS format  
   function glp_write_maxflow
     (G : access glp_graph;
      s : int;
      t : int;
      a_cap : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1122
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_maxflow";

  -- write maximum flow problem data in DIMACS format  
   function glp_read_asnprob
     (G : access glp_graph;
      v_set : int;
      a_cost : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1126
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_asnprob";

  -- read assignment problem data in DIMACS format  
   function glp_write_asnprob
     (G : access glp_graph;
      v_set : int;
      a_cost : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1130
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_asnprob";

  -- write assignment problem data in DIMACS format  
   function glp_read_ccdata
     (G : access glp_graph;
      v_wgt : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1134
   with Import => True, 
        Convention => C, 
        External_Name => "glp_read_ccdata";

  -- read graph in DIMACS clique/coloring format  
   function glp_write_ccdata
     (G : access glp_graph;
      v_wgt : int;
      fname : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/glpk.h:1137
   with Import => True, 
        Convention => C, 
        External_Name => "glp_write_ccdata";

  -- write graph in DIMACS clique/coloring format  
   function glp_netgen
     (G : access glp_graph;
      v_rhs : int;
      a_cap : int;
      a_cost : int;
      parm : access int) return int  -- /usr/include/glpk.h:1140
   with Import => True, 
        Convention => C, 
        External_Name => "glp_netgen";

  -- Klingman's network problem generator  
   procedure glp_netgen_prob (nprob : int; parm : access int)  -- /usr/include/glpk.h:1144
   with Import => True, 
        Convention => C, 
        External_Name => "glp_netgen_prob";

  -- Klingman's standard network problem instance  
   function glp_gridgen
     (G : access glp_graph;
      v_rhs : int;
      a_cap : int;
      a_cost : int;
      parm : access int) return int  -- /usr/include/glpk.h:1147
   with Import => True, 
        Convention => C, 
        External_Name => "glp_gridgen";

  -- grid-like network problem generator  
   function glp_rmfgen
     (G : access glp_graph;
      s : access int;
      t : access int;
      a_cap : int;
      parm : access int) return int  -- /usr/include/glpk.h:1151
   with Import => True, 
        Convention => C, 
        External_Name => "glp_rmfgen";

  -- Goldfarb's maximum flow problem generator  
   function glp_weak_comp (G : access glp_graph; v_num : int) return int  -- /usr/include/glpk.h:1155
   with Import => True, 
        Convention => C, 
        External_Name => "glp_weak_comp";

  -- find all weakly connected components of graph  
   function glp_strong_comp (G : access glp_graph; v_num : int) return int  -- /usr/include/glpk.h:1158
   with Import => True, 
        Convention => C, 
        External_Name => "glp_strong_comp";

  -- find all strongly connected components of graph  
   function glp_top_sort (G : access glp_graph; v_num : int) return int  -- /usr/include/glpk.h:1161
   with Import => True, 
        Convention => C, 
        External_Name => "glp_top_sort";

  -- topological sorting of acyclic digraph  
   function glp_wclique_exact
     (G : access glp_graph;
      v_wgt : int;
      sol : access double;
      v_set : int) return int  -- /usr/include/glpk.h:1164
   with Import => True, 
        Convention => C, 
        External_Name => "glp_wclique_exact";

  -- find maximum weight clique with exact algorithm  
  -- eof  
end glpk_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
