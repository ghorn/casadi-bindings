{-# OPTIONS_GHC -Wall #-}

module Casadi.Core.Tools
       (
         casadi_abs__0,
         casadi_abs__1,
         casadi_abs__2,
         casadi_abs__3,
         casadi_abs__4,
         casadi_acos__0,
         casadi_acos__1,
         casadi_acos__2,
         casadi_acos__3,
         casadi_acos__4,
         casadi_acosh__0,
         casadi_acosh__1,
         casadi_acosh__2,
         casadi_acosh__3,
         casadi_acosh__4,
         casadi_adj__0,
         casadi_adj__1,
         casadi_adj__2,
         casadi_all__0,
         casadi_all__1,
         casadi_all__2,
         casadi_and__0,
         casadi_and__1,
         casadi_and__2,
         casadi_and__3,
         casadi_and__4,
         casadi_any__0,
         casadi_any__1,
         casadi_any__2,
         casadi_asin__0,
         casadi_asin__1,
         casadi_asin__2,
         casadi_asin__3,
         casadi_asin__4,
         casadi_asinh__0,
         casadi_asinh__1,
         casadi_asinh__2,
         casadi_asinh__3,
         casadi_asinh__4,
         casadi_atan2__0,
         casadi_atan2__1,
         casadi_atan2__2,
         casadi_atan2__3,
         casadi_atan2__4,
         casadi_atan__0,
         casadi_atan__1,
         casadi_atan__2,
         casadi_atan__3,
         casadi_atan__4,
         casadi_atanh__0,
         casadi_atanh__1,
         casadi_atanh__2,
         casadi_atanh__3,
         casadi_atanh__4,
         casadi_bilin__0,
         casadi_bilin__1,
         casadi_bilin__2,
         casadi_bilin__3,
         casadi_blockcat__0,
         casadi_blockcat__1,
         casadi_blockcat__2,
         casadi_blockcat__3,
         casadi_blockcat__4,
         casadi_blockcat__5,
         casadi_blockcat__6,
         casadi_blockcat__7,
         casadi_blockcat__8,
         casadi_blockcat__9,
         casadi_blocksplit__0,
         casadi_blocksplit__1,
         casadi_blocksplit__10,
         casadi_blocksplit__11,
         casadi_blocksplit__12,
         casadi_blocksplit__13,
         casadi_blocksplit__14,
         casadi_blocksplit__15,
         casadi_blocksplit__16,
         casadi_blocksplit__17,
         casadi_blocksplit__18,
         casadi_blocksplit__19,
         casadi_blocksplit__2,
         casadi_blocksplit__3,
         casadi_blocksplit__4,
         casadi_blocksplit__5,
         casadi_blocksplit__6,
         casadi_blocksplit__7,
         casadi_blocksplit__8,
         casadi_blocksplit__9,
         casadi_ceil__0,
         casadi_ceil__1,
         casadi_ceil__2,
         casadi_ceil__3,
         casadi_ceil__4,
         casadi_chol__0,
         casadi_chol__1,
         casadi_chol__2,
         casadi_cofactor__0,
         casadi_cofactor__1,
         casadi_cofactor__2,
         casadi_conditional__0,
         casadi_conditional__1,
         casadi_conditional__2,
         casadi_conditional__3,
         casadi_conditional__4,
         casadi_conditional__5,
         casadi_conditional__6,
         casadi_conditional__7,
         casadi_constpow__0,
         casadi_constpow__1,
         casadi_constpow__2,
         casadi_constpow__3,
         casadi_constpow__4,
         casadi_copysign__0,
         casadi_copysign__1,
         casadi_copysign__2,
         casadi_copysign__3,
         casadi_copysign__4,
         casadi_cos__0,
         casadi_cos__1,
         casadi_cos__2,
         casadi_cos__3,
         casadi_cos__4,
         casadi_cosh__0,
         casadi_cosh__1,
         casadi_cosh__2,
         casadi_cosh__3,
         casadi_cosh__4,
         casadi_cross__0,
         casadi_cross__1,
         casadi_cross__2,
         casadi_cross__3,
         casadi_cross__4,
         casadi_cross__5,
         casadi_cross__6,
         casadi_cross__7,
         casadi_densify__0,
         casadi_densify__1,
         casadi_densify__2,
         casadi_densify__3,
         casadi_depends_on__0,
         casadi_depends_on__1,
         casadi_depends_on__2,
         casadi_depends_on__3,
         casadi_det__0,
         casadi_det__1,
         casadi_det__2,
         casadi_det__3,
         casadi_diag__0,
         casadi_diag__1,
         casadi_diag__2,
         casadi_diag__3,
         casadi_diagcat__0,
         casadi_diagcat__1,
         casadi_diagcat__2,
         casadi_diagcat__3,
         casadi_diagcat__4,
         casadi_diagsplit__0,
         casadi_diagsplit__1,
         casadi_diagsplit__10,
         casadi_diagsplit__11,
         casadi_diagsplit__12,
         casadi_diagsplit__13,
         casadi_diagsplit__14,
         casadi_diagsplit__15,
         casadi_diagsplit__16,
         casadi_diagsplit__17,
         casadi_diagsplit__18,
         casadi_diagsplit__19,
         casadi_diagsplit__2,
         casadi_diagsplit__20,
         casadi_diagsplit__21,
         casadi_diagsplit__22,
         casadi_diagsplit__23,
         casadi_diagsplit__24,
         casadi_diagsplit__3,
         casadi_diagsplit__4,
         casadi_diagsplit__5,
         casadi_diagsplit__6,
         casadi_diagsplit__7,
         casadi_diagsplit__8,
         casadi_diagsplit__9,
         casadi_dot__0,
         casadi_dot__1,
         casadi_dot__2,
         casadi_dot__3,
         casadi_eig_symbolic__0,
         casadi_eig_symbolic__1,
         casadi_eig_symbolic__2,
         casadi_eq__0,
         casadi_eq__1,
         casadi_eq__2,
         casadi_eq__3,
         casadi_eq__4,
         casadi_erf__0,
         casadi_erf__1,
         casadi_erf__2,
         casadi_erf__3,
         casadi_erf__4,
         casadi_erfinv__0,
         casadi_erfinv__1,
         casadi_erfinv__2,
         casadi_erfinv__3,
         casadi_erfinv__4,
         casadi_exp__0,
         casadi_exp__1,
         casadi_exp__2,
         casadi_exp__3,
         casadi_exp__4,
         casadi_expand__0,
         casadi_expand__1,
         casadi_expand__2,
         casadi_find,
         casadi_floor__0,
         casadi_floor__1,
         casadi_floor__2,
         casadi_floor__3,
         casadi_floor__4,
         casadi_forward__0,
         casadi_forward__1,
         casadi_forward__2,
         casadi_forward__3,
         casadi_forward__4,
         casadi_forward__5,
         casadi_forward__6,
         casadi_forward__7,
         casadi_gauss_quadrature__0,
         casadi_gauss_quadrature__1,
         casadi_gauss_quadrature__2,
         casadi_gauss_quadrature__3,
         casadi_gauss_quadrature__4,
         casadi_gauss_quadrature__5,
         casadi_gauss_quadrature__6,
         casadi_gauss_quadrature__7,
         casadi_gauss_quadrature__8,
         casadi_ge__0,
         casadi_ge__1,
         casadi_ge__2,
         casadi_ge__3,
         casadi_ge__4,
         casadi_getMinor__0,
         casadi_getMinor__1,
         casadi_getMinor__2,
         casadi_gradient__0,
         casadi_gradient__1,
         casadi_gradient__2,
         casadi_gradient__3,
         casadi_graph_substitute__0,
         casadi_graph_substitute__1,
         casadi_gt__0,
         casadi_gt__1,
         casadi_gt__2,
         casadi_gt__3,
         casadi_gt__4,
         casadi_heaviside__0,
         casadi_heaviside__1,
         casadi_heaviside__2,
         casadi_hessian__0,
         casadi_hessian__1,
         casadi_hessian__2,
         casadi_hessian__3,
         casadi_horzcat__0,
         casadi_horzcat__1,
         casadi_horzcat__2,
         casadi_horzcat__3,
         casadi_horzcat__4,
         casadi_horzsplit__0,
         casadi_horzsplit__1,
         casadi_horzsplit__10,
         casadi_horzsplit__11,
         casadi_horzsplit__12,
         casadi_horzsplit__13,
         casadi_horzsplit__14,
         casadi_horzsplit__2,
         casadi_horzsplit__3,
         casadi_horzsplit__4,
         casadi_horzsplit__5,
         casadi_horzsplit__6,
         casadi_horzsplit__7,
         casadi_horzsplit__8,
         casadi_horzsplit__9,
         casadi_if_else__0,
         casadi_if_else__1,
         casadi_if_else__2,
         casadi_if_else__3,
         casadi_if_else__4,
         casadi_if_else__5,
         casadi_if_else__6,
         casadi_if_else__7,
         casadi_inv__0,
         casadi_inv__1,
         casadi_inv__2,
         casadi_inv__3,
         casadi_inv_skew__0,
         casadi_inv_skew__1,
         casadi_inv_skew__2,
         casadi_inv_skew__3,
         casadi_is_equal__0,
         casadi_is_equal__1,
         casadi_is_equal__2,
         casadi_is_equal__3,
         casadi_is_equal__4,
         casadi_is_equal__5,
         casadi_is_equal__6,
         casadi_is_equal__7,
         casadi_is_equal__8,
         casadi_is_equal__9,
         casadi_jacobian__0,
         casadi_jacobian__1,
         casadi_jacobian__2,
         casadi_jacobian__3,
         casadi_jtimes__0,
         casadi_jtimes__1,
         casadi_jtimes__2,
         casadi_jtimes__3,
         casadi_jtimes__4,
         casadi_jtimes__5,
         casadi_jtimes__6,
         casadi_jtimes__7,
         casadi_kron__0,
         casadi_kron__1,
         casadi_kron__2,
         casadi_kron__3,
         casadi_kron__4,
         casadi_ldivide__0,
         casadi_ldivide__1,
         casadi_ldivide__2,
         casadi_ldivide__3,
         casadi_ldivide__4,
         casadi_le__0,
         casadi_le__1,
         casadi_le__2,
         casadi_le__3,
         casadi_le__4,
         casadi_linspace__0,
         casadi_linspace__1,
         casadi_linspace__2,
         casadi_linspace__3,
         casadi_log10__0,
         casadi_log10__1,
         casadi_log10__2,
         casadi_log10__3,
         casadi_log10__4,
         casadi_log__0,
         casadi_log__1,
         casadi_log__2,
         casadi_log__3,
         casadi_log__4,
         casadi_lt__0,
         casadi_lt__1,
         casadi_lt__2,
         casadi_lt__3,
         casadi_lt__4,
         casadi_mac__0,
         casadi_mac__1,
         casadi_mac__2,
         casadi_mac__3,
         casadi_mac__4,
         casadi_matrix_expand__0,
         casadi_matrix_expand__1,
         casadi_matrix_expand__2,
         casadi_matrix_expand__3,
         casadi_matrix_expand__4,
         casadi_matrix_expand__5,
         casadi_max__0,
         casadi_max__1,
         casadi_max__2,
         casadi_max__3,
         casadi_max__4,
         casadi_min__0,
         casadi_min__1,
         casadi_min__2,
         casadi_min__3,
         casadi_min__4,
         casadi_minus__0,
         casadi_minus__1,
         casadi_minus__2,
         casadi_minus__3,
         casadi_minus__4,
         casadi_mldivide__0,
         casadi_mldivide__1,
         casadi_mldivide__2,
         casadi_mldivide__3,
         casadi_mod__0,
         casadi_mod__1,
         casadi_mod__2,
         casadi_mod__3,
         casadi_mod__4,
         casadi_mpower__0,
         casadi_mpower__1,
         casadi_mpower__2,
         casadi_mpower__3,
         casadi_mrdivide__0,
         casadi_mrdivide__1,
         casadi_mrdivide__2,
         casadi_mrdivide__3,
         casadi_mtaylor__0,
         casadi_mtaylor__1,
         casadi_mtaylor__2,
         casadi_mtaylor__3,
         casadi_mtaylor__4,
         casadi_mtaylor__5,
         casadi_mtaylor__6,
         casadi_mtaylor__7,
         casadi_mtaylor__8,
         casadi_mtimes__0,
         casadi_mtimes__1,
         casadi_mtimes__2,
         casadi_mtimes__3,
         casadi_mtimes__4,
         casadi_mtimes__5,
         casadi_mtimes__6,
         casadi_mtimes__7,
         casadi_mtimes__8,
         casadi_mtimes__9,
         casadi_n_nodes__0,
         casadi_n_nodes__1,
         casadi_n_nodes__2,
         casadi_n_nodes__3,
         casadi_ne__0,
         casadi_ne__1,
         casadi_ne__2,
         casadi_ne__3,
         casadi_ne__4,
         casadi_norm_0_mul__0,
         casadi_norm_0_mul__1,
         casadi_norm_0_mul__2,
         casadi_norm_0_mul__3,
         casadi_norm_0_mul__4,
         casadi_norm_1__0,
         casadi_norm_1__1,
         casadi_norm_1__2,
         casadi_norm_1__3,
         casadi_norm_2__0,
         casadi_norm_2__1,
         casadi_norm_2__2,
         casadi_norm_2__3,
         casadi_norm_fro__0,
         casadi_norm_fro__1,
         casadi_norm_fro__2,
         casadi_norm_fro__3,
         casadi_norm_inf__0,
         casadi_norm_inf__1,
         casadi_norm_inf__2,
         casadi_norm_inf__3,
         casadi_norm_inf_mul__0,
         casadi_norm_inf_mul__1,
         casadi_norm_inf_mul__2,
         casadi_not__0,
         casadi_not__1,
         casadi_not__2,
         casadi_not__3,
         casadi_not__4,
         casadi_nullspace__0,
         casadi_nullspace__1,
         casadi_nullspace__2,
         casadi_nullspace__3,
         casadi_offset__0,
         casadi_offset__1,
         casadi_offset__2,
         casadi_offset__3,
         casadi_offset__4,
         casadi_offset__5,
         casadi_offset__6,
         casadi_offset__7,
         casadi_offset__8,
         casadi_offset__9,
         casadi_or__0,
         casadi_or__1,
         casadi_or__2,
         casadi_or__3,
         casadi_or__4,
         casadi_pinv__0,
         casadi_pinv__1,
         casadi_pinv__10,
         casadi_pinv__11,
         casadi_pinv__2,
         casadi_pinv__3,
         casadi_pinv__4,
         casadi_pinv__5,
         casadi_pinv__6,
         casadi_pinv__7,
         casadi_pinv__8,
         casadi_pinv__9,
         casadi_plus__0,
         casadi_plus__1,
         casadi_plus__2,
         casadi_plus__3,
         casadi_plus__4,
         casadi_poly_coeff__0,
         casadi_poly_coeff__1,
         casadi_poly_coeff__2,
         casadi_poly_roots__0,
         casadi_poly_roots__1,
         casadi_poly_roots__2,
         casadi_polyval__0,
         casadi_polyval__1,
         casadi_polyval__2,
         casadi_polyval__3,
         casadi_power__0,
         casadi_power__1,
         casadi_power__2,
         casadi_power__3,
         casadi_power__4,
         casadi_print_operator__0,
         casadi_print_operator__1,
         casadi_print_operator__2,
         casadi_print_operator__3,
         casadi_project__0,
         casadi_project__1,
         casadi_project__2,
         casadi_project__3,
         casadi_project__4,
         casadi_project__5,
         casadi_project__6,
         casadi_project__7,
         casadi_pw_const__0,
         casadi_pw_const__1,
         casadi_pw_const__2,
         casadi_pw_lin__0,
         casadi_pw_lin__1,
         casadi_pw_lin__2,
         casadi_qr__0,
         casadi_qr__1,
         casadi_qr__2,
         casadi_ramp__0,
         casadi_ramp__1,
         casadi_ramp__2,
         casadi_rank1__0,
         casadi_rank1__1,
         casadi_rank1__2,
         casadi_rank1__3,
         casadi_rdivide__0,
         casadi_rdivide__1,
         casadi_rdivide__2,
         casadi_rdivide__3,
         casadi_rdivide__4,
         casadi_rectangle__0,
         casadi_rectangle__1,
         casadi_rectangle__2,
         casadi_repmat__0,
         casadi_repmat__1,
         casadi_repmat__10,
         casadi_repmat__11,
         casadi_repmat__12,
         casadi_repmat__13,
         casadi_repmat__14,
         casadi_repmat__2,
         casadi_repmat__3,
         casadi_repmat__4,
         casadi_repmat__5,
         casadi_repmat__6,
         casadi_repmat__7,
         casadi_repmat__8,
         casadi_repmat__9,
         casadi_repsum__0,
         casadi_repsum__1,
         casadi_repsum__2,
         casadi_repsum__3,
         casadi_repsum__4,
         casadi_repsum__5,
         casadi_repsum__6,
         casadi_repsum__7,
         casadi_reshape__0,
         casadi_reshape__1,
         casadi_reshape__10,
         casadi_reshape__11,
         casadi_reshape__12,
         casadi_reshape__13,
         casadi_reshape__14,
         casadi_reshape__2,
         casadi_reshape__3,
         casadi_reshape__4,
         casadi_reshape__5,
         casadi_reshape__6,
         casadi_reshape__7,
         casadi_reshape__8,
         casadi_reshape__9,
         casadi_reverse__0,
         casadi_reverse__1,
         casadi_reverse__2,
         casadi_reverse__3,
         casadi_reverse__4,
         casadi_reverse__5,
         casadi_reverse__6,
         casadi_reverse__7,
         casadi_shared__0,
         casadi_shared__1,
         casadi_shared__10,
         casadi_shared__11,
         casadi_shared__2,
         casadi_shared__3,
         casadi_shared__4,
         casadi_shared__5,
         casadi_shared__6,
         casadi_shared__7,
         casadi_shared__8,
         casadi_shared__9,
         casadi_sign__0,
         casadi_sign__1,
         casadi_sign__2,
         casadi_sign__3,
         casadi_sign__4,
         casadi_simplify__0,
         casadi_simplify__1,
         casadi_simplify__2,
         casadi_simplify__3,
         casadi_simplify__4,
         casadi_sin__0,
         casadi_sin__1,
         casadi_sin__2,
         casadi_sin__3,
         casadi_sin__4,
         casadi_sinh__0,
         casadi_sinh__1,
         casadi_sinh__2,
         casadi_sinh__3,
         casadi_sinh__4,
         casadi_skew__0,
         casadi_skew__1,
         casadi_skew__2,
         casadi_skew__3,
         casadi_solve__0,
         casadi_solve__1,
         casadi_solve__10,
         casadi_solve__11,
         casadi_solve__2,
         casadi_solve__3,
         casadi_solve__4,
         casadi_solve__5,
         casadi_solve__6,
         casadi_solve__7,
         casadi_solve__8,
         casadi_solve__9,
         casadi_sparsify__0,
         casadi_sparsify__1,
         casadi_sparsify__2,
         casadi_sparsify__3,
         casadi_sparsify__4,
         casadi_sparsify__5,
         casadi_sprank__0,
         casadi_sprank__1,
         casadi_sprank__2,
         casadi_sprank__3,
         casadi_sprank__4,
         casadi_sqrt__0,
         casadi_sqrt__1,
         casadi_sqrt__2,
         casadi_sqrt__3,
         casadi_sqrt__4,
         casadi_substitute__0,
         casadi_substitute__1,
         casadi_substitute__2,
         casadi_substitute__3,
         casadi_substitute__4,
         casadi_substitute__5,
         casadi_substitute__6,
         casadi_substitute__7,
         casadi_substitute_inplace__0,
         casadi_substitute_inplace__1,
         casadi_substitute_inplace__2,
         casadi_substitute_inplace__3,
         casadi_substitute_inplace__4,
         casadi_substitute_inplace__5,
         casadi_substitute_inplace__6,
         casadi_substitute_inplace__7,
         casadi_sum1__0,
         casadi_sum1__1,
         casadi_sum1__2,
         casadi_sum1__3,
         casadi_sum2__0,
         casadi_sum2__1,
         casadi_sum2__2,
         casadi_sum2__3,
         casadi_sum_square__0,
         casadi_sum_square__1,
         casadi_sum_square__2,
         casadi_sum_square__3,
         casadi_symvar__0,
         casadi_symvar__1,
         casadi_symvar__2,
         casadi_symvar__3,
         casadi_tan__0,
         casadi_tan__1,
         casadi_tan__2,
         casadi_tan__3,
         casadi_tan__4,
         casadi_tangent__0,
         casadi_tangent__1,
         casadi_tangent__2,
         casadi_tangent__3,
         casadi_tanh__0,
         casadi_tanh__1,
         casadi_tanh__2,
         casadi_tanh__3,
         casadi_tanh__4,
         casadi_taylor__0,
         casadi_taylor__1,
         casadi_taylor__2,
         casadi_taylor__3,
         casadi_taylor__4,
         casadi_taylor__5,
         casadi_taylor__6,
         casadi_taylor__7,
         casadi_taylor__8,
         casadi_times__0,
         casadi_times__1,
         casadi_times__2,
         casadi_times__3,
         casadi_times__4,
         casadi_trace__0,
         casadi_trace__1,
         casadi_trace__2,
         casadi_trace__3,
         casadi_transpose__0,
         casadi_transpose__1,
         casadi_transpose__2,
         casadi_transpose__3,
         casadi_transpose__4,
         casadi_triangle__0,
         casadi_triangle__1,
         casadi_triangle__2,
         casadi_tril2symm__0,
         casadi_tril2symm__1,
         casadi_tril2symm__2,
         casadi_tril2symm__3,
         casadi_tril__0,
         casadi_tril__1,
         casadi_tril__2,
         casadi_tril__3,
         casadi_tril__4,
         casadi_tril__5,
         casadi_tril__6,
         casadi_tril__7,
         casadi_tril__8,
         casadi_tril__9,
         casadi_triu2symm__0,
         casadi_triu2symm__1,
         casadi_triu2symm__2,
         casadi_triu2symm__3,
         casadi_triu__0,
         casadi_triu__1,
         casadi_triu__2,
         casadi_triu__3,
         casadi_triu__4,
         casadi_triu__5,
         casadi_triu__6,
         casadi_triu__7,
         casadi_triu__8,
         casadi_triu__9,
         casadi_unite__0,
         casadi_unite__1,
         casadi_unite__2,
         casadi_unite__3,
         casadi_vec__0,
         casadi_vec__1,
         casadi_vec__2,
         casadi_vec__3,
         casadi_vec__4,
         casadi_veccat__0,
         casadi_veccat__1,
         casadi_veccat__2,
         casadi_veccat__3,
         casadi_veccat__4,
         casadi_vertcat__0,
         casadi_vertcat__1,
         casadi_vertcat__2,
         casadi_vertcat__3,
         casadi_vertcat__4,
         casadi_vertsplit__0,
         casadi_vertsplit__1,
         casadi_vertsplit__10,
         casadi_vertsplit__11,
         casadi_vertsplit__12,
         casadi_vertsplit__13,
         casadi_vertsplit__14,
         casadi_vertsplit__2,
         casadi_vertsplit__3,
         casadi_vertsplit__4,
         casadi_vertsplit__5,
         casadi_vertsplit__6,
         casadi_vertsplit__7,
         casadi_vertsplit__8,
         casadi_vertsplit__9,
         casadi_which_depends__0,
         casadi_which_depends__1,
         casadi_which_depends__2,
         casadi_which_depends__3,
         collocation_interpolators,
         collocation_points__0,
         collocation_points__1,
         complement,
         conic__0,
         conic__1,
         conic_in__0,
         conic_in__1,
         conic_n_in,
         conic_n_out,
         conic_out__0,
         conic_out__1,
         doc_conic,
         doc_dple,
         doc_integrator,
         doc_interpolant,
         doc_nlpsol,
         doc_rootfinder,
         dple_in__0,
         dple_in__1,
         dple_n_in,
         dple_n_out,
         dple_out__0,
         dple_out__1,
         dplesol__0,
         dplesol__1,
         dplesol__2,
         dplesol__3,
         dplesol__4,
         dplesol__5,
         dplesol__6,
         dplesol__7,
         external__0,
         external__1,
         external__2,
         external__3,
         external__4,
         external__5,
         has_conic,
         has_dple,
         has_integrator,
         has_interpolant,
         has_nlpsol,
         has_rootfinder,
         hash_combine,
         hash_sparsity,
         integrator__0,
         integrator__1,
         integrator__2,
         integrator__3,
         integrator_in__0,
         integrator_in__1,
         integrator_n_in,
         integrator_n_out,
         integrator_out__0,
         integrator_out__1,
         interpolant__0,
         interpolant__1,
         is_slice2,
         is_slice__0,
         is_slice__1,
         is_slice__2,
         is_slice__3,
         jit__0,
         jit__1,
         load_conic,
         load_dple,
         load_integrator,
         load_interpolant,
         load_nlpsol,
         load_rootfinder,
         lookupvector,
         nlpsol__0,
         nlpsol__1,
         nlpsol__2,
         nlpsol__3,
         nlpsol__4,
         nlpsol__5,
         nlpsol__6,
         nlpsol__7,
         nlpsol__8,
         nlpsol__9,
         nlpsol_default_in__0,
         nlpsol_default_in__1,
         nlpsol_in__0,
         nlpsol_in__1,
         nlpsol_n_in,
         nlpsol_n_out,
         nlpsol_out__0,
         nlpsol_out__1,
         qpsol__0,
         qpsol__1,
         qpsol__2,
         qpsol__3,
         rootfinder__0,
         rootfinder__1,
         simpleIRK__0,
         simpleIRK__1,
         simpleIRK__2,
         simpleIRK__3,
         simpleIRK__4,
         simpleIRK__5,
         simpleIntegrator__0,
         simpleIntegrator__1,
         simpleIntegrator__2,
         simpleRK__0,
         simpleRK__1,
         simpleRK__2,
         to_slice2,
         to_slice__0,
         to_slice__1,
         to_slice__2,
         to_slice__3,
       ) where


import Data.Vector ( Vector )
import qualified Data.Map as M
import Foreign.C.Types
import Foreign.Marshal ( new, free )
import Foreign.Storable ( peek )
import Foreign.Ptr ( Ptr, nullPtr )

import Casadi.Core.Data
import Casadi.Core.Enums
import Casadi.Internal.FormatException ( formatException )
import Casadi.Internal.MarshalTypes ( StdMap, StdVec, StdString, StdPair )
import Casadi.Internal.Marshal ( Marshal(..) )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
foreign import ccall unsafe "casadi_abs__0" c_casadi_abs__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_abs__0
  :: Double -> IO Double
casadi_abs__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_abs__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_abs__1" c_casadi_abs__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_abs__1
  :: SX -> IO SX
casadi_abs__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_abs__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_abs__2" c_casadi_abs__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_abs__2
  :: DM -> IO DM
casadi_abs__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_abs__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_abs__3" c_casadi_abs__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_abs__3
  :: IM -> IO IM
casadi_abs__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_abs__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_abs__4" c_casadi_abs__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_abs__4
  :: MX -> IO MX
casadi_abs__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_abs__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acos__0" c_casadi_acos__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_acos__0
  :: Double -> IO Double
casadi_acos__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acos__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acos__1" c_casadi_acos__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_acos__1
  :: SX -> IO SX
casadi_acos__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acos__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acos__2" c_casadi_acos__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_acos__2
  :: DM -> IO DM
casadi_acos__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acos__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acos__3" c_casadi_acos__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_acos__3
  :: IM -> IO IM
casadi_acos__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acos__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acos__4" c_casadi_acos__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_acos__4
  :: MX -> IO MX
casadi_acos__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acos__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acosh__0" c_casadi_acosh__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_acosh__0
  :: Double -> IO Double
casadi_acosh__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acosh__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acosh__1" c_casadi_acosh__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_acosh__1
  :: SX -> IO SX
casadi_acosh__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acosh__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acosh__2" c_casadi_acosh__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_acosh__2
  :: DM -> IO DM
casadi_acosh__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acosh__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acosh__3" c_casadi_acosh__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_acosh__3
  :: IM -> IO IM
casadi_acosh__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acosh__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_acosh__4" c_casadi_acosh__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_acosh__4
  :: MX -> IO MX
casadi_acosh__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_acosh__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_adj__0" c_casadi_adj__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_adj__0
  :: SX -> IO SX
casadi_adj__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_adj__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_adj__1" c_casadi_adj__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_adj__1
  :: DM -> IO DM
casadi_adj__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_adj__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_adj__2" c_casadi_adj__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_adj__2
  :: IM -> IO IM
casadi_adj__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_adj__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_all__0" c_casadi_all__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_all__0
  :: SX -> IO SX
casadi_all__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_all__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_all__1" c_casadi_all__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_all__1
  :: DM -> IO DM
casadi_all__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_all__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_all__2" c_casadi_all__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_all__2
  :: IM -> IO IM
casadi_all__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_all__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_and__0" c_casadi_and__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_and__0
  :: Double -> Double -> IO Double
casadi_and__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_and__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_and__1" c_casadi_and__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_and__1
  :: SX -> SX -> IO SX
casadi_and__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_and__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_and__2" c_casadi_and__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_and__2
  :: DM -> DM -> IO DM
casadi_and__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_and__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_and__3" c_casadi_and__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_and__3
  :: IM -> IM -> IO IM
casadi_and__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_and__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_and__4" c_casadi_and__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_and__4
  :: MX -> MX -> IO MX
casadi_and__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_and__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_any__0" c_casadi_any__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_any__0
  :: SX -> IO SX
casadi_any__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_any__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_any__1" c_casadi_any__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_any__1
  :: DM -> IO DM
casadi_any__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_any__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_any__2" c_casadi_any__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_any__2
  :: IM -> IO IM
casadi_any__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_any__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asin__0" c_casadi_asin__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_asin__0
  :: Double -> IO Double
casadi_asin__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asin__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asin__1" c_casadi_asin__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_asin__1
  :: SX -> IO SX
casadi_asin__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asin__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asin__2" c_casadi_asin__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_asin__2
  :: DM -> IO DM
casadi_asin__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asin__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asin__3" c_casadi_asin__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_asin__3
  :: IM -> IO IM
casadi_asin__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asin__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asin__4" c_casadi_asin__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_asin__4
  :: MX -> IO MX
casadi_asin__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asin__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asinh__0" c_casadi_asinh__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_asinh__0
  :: Double -> IO Double
casadi_asinh__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asinh__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asinh__1" c_casadi_asinh__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_asinh__1
  :: SX -> IO SX
casadi_asinh__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asinh__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asinh__2" c_casadi_asinh__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_asinh__2
  :: DM -> IO DM
casadi_asinh__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asinh__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asinh__3" c_casadi_asinh__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_asinh__3
  :: IM -> IO IM
casadi_asinh__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asinh__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_asinh__4" c_casadi_asinh__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_asinh__4
  :: MX -> IO MX
casadi_asinh__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_asinh__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atan__0" c_casadi_atan__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_atan__0
  :: Double -> IO Double
casadi_atan__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atan__1" c_casadi_atan__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_atan__1
  :: SX -> IO SX
casadi_atan__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atan__2" c_casadi_atan__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_atan__2
  :: DM -> IO DM
casadi_atan__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atan__3" c_casadi_atan__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_atan__3
  :: IM -> IO IM
casadi_atan__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atan__4" c_casadi_atan__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_atan__4
  :: MX -> IO MX
casadi_atan__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atan2__0" c_casadi_atan2__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_atan2__0
  :: Double -> Double -> IO Double
casadi_atan2__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan2__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_atan2__1" c_casadi_atan2__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_atan2__1
  :: SX -> SX -> IO SX
casadi_atan2__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan2__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_atan2__2" c_casadi_atan2__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_atan2__2
  :: DM -> DM -> IO DM
casadi_atan2__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan2__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_atan2__3" c_casadi_atan2__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_atan2__3
  :: IM -> IM -> IO IM
casadi_atan2__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan2__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_atan2__4" c_casadi_atan2__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_atan2__4
  :: MX -> MX -> IO MX
casadi_atan2__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atan2__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_atanh__0" c_casadi_atanh__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_atanh__0
  :: Double -> IO Double
casadi_atanh__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atanh__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atanh__1" c_casadi_atanh__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_atanh__1
  :: SX -> IO SX
casadi_atanh__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atanh__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atanh__2" c_casadi_atanh__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_atanh__2
  :: DM -> IO DM
casadi_atanh__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atanh__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atanh__3" c_casadi_atanh__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_atanh__3
  :: IM -> IO IM
casadi_atanh__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atanh__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_atanh__4" c_casadi_atanh__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_atanh__4
  :: MX -> IO MX
casadi_atanh__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_atanh__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_bilin__0" c_casadi_bilin__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_bilin__0
  :: SX -> SX -> SX -> IO SX
casadi_bilin__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_bilin__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_bilin__1" c_casadi_bilin__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_bilin__1
  :: DM -> DM -> DM -> IO DM
casadi_bilin__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_bilin__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_bilin__2" c_casadi_bilin__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_bilin__2
  :: IM -> IM -> IM -> IO IM
casadi_bilin__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_bilin__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_bilin__3" c_casadi_bilin__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_bilin__3
  :: MX -> MX -> MX -> IO MX
casadi_bilin__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_bilin__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blockcat__0" c_casadi_blockcat__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_blockcat__0
  :: SX -> SX -> SX -> SX -> IO SX
casadi_blockcat__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_blockcat__1" c_casadi_blockcat__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec (Ptr SX')))) -> IO (Ptr SX')

casadi_blockcat__1
  :: Vector (Vector SX) -> IO SX
casadi_blockcat__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blockcat__2" c_casadi_blockcat__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_blockcat__2
  :: DM -> DM -> DM -> DM -> IO DM
casadi_blockcat__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_blockcat__3" c_casadi_blockcat__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec (Ptr DM')))) -> IO (Ptr DM')

casadi_blockcat__3
  :: Vector (Vector DM) -> IO DM
casadi_blockcat__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blockcat__4" c_casadi_blockcat__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_blockcat__4
  :: IM -> IM -> IM -> IM -> IO IM
casadi_blockcat__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_blockcat__5" c_casadi_blockcat__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec (Ptr IM')))) -> IO (Ptr IM')

casadi_blockcat__5
  :: Vector (Vector IM) -> IO IM
casadi_blockcat__5 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__5 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blockcat__6" c_casadi_blockcat__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_blockcat__6
  :: MX -> MX -> MX -> MX -> IO MX
casadi_blockcat__6 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__6 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_blockcat__7" c_casadi_blockcat__7
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec (Ptr MX')))) -> IO (Ptr MX')

casadi_blockcat__7
  :: Vector (Vector MX) -> IO MX
casadi_blockcat__7 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__7 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blockcat__8" c_casadi_blockcat__8
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_blockcat__8
  :: Sparsity -> Sparsity -> Sparsity -> Sparsity -> IO Sparsity
casadi_blockcat__8 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__8 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_blockcat__9" c_casadi_blockcat__9
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec (Ptr Sparsity')))) -> IO (Ptr Sparsity')

casadi_blockcat__9
  :: Vector (Vector Sparsity) -> IO Sparsity
casadi_blockcat__9 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blockcat__9 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blocksplit__0" c_casadi_blocksplit__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_blocksplit__0
  :: SX -> IO (Vector (Vector SX))
casadi_blocksplit__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blocksplit__1" c_casadi_blocksplit__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_blocksplit__1
  :: SX -> Int -> IO (Vector (Vector SX))
casadi_blocksplit__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_blocksplit__2" c_casadi_blocksplit__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_blocksplit__2
  :: SX -> Int -> Int -> IO (Vector (Vector SX))
casadi_blocksplit__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__3" c_casadi_blocksplit__3
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_blocksplit__3
  :: SX -> Vector Int -> Vector Int -> IO (Vector (Vector SX))
casadi_blocksplit__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__4" c_casadi_blocksplit__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_blocksplit__4
  :: DM -> IO (Vector (Vector DM))
casadi_blocksplit__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blocksplit__5" c_casadi_blocksplit__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_blocksplit__5
  :: DM -> Int -> IO (Vector (Vector DM))
casadi_blocksplit__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_blocksplit__6" c_casadi_blocksplit__6
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_blocksplit__6
  :: DM -> Int -> Int -> IO (Vector (Vector DM))
casadi_blocksplit__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__7" c_casadi_blocksplit__7
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_blocksplit__7
  :: DM -> Vector Int -> Vector Int -> IO (Vector (Vector DM))
casadi_blocksplit__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__8" c_casadi_blocksplit__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_blocksplit__8
  :: IM -> IO (Vector (Vector IM))
casadi_blocksplit__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blocksplit__9" c_casadi_blocksplit__9
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_blocksplit__9
  :: IM -> Int -> IO (Vector (Vector IM))
casadi_blocksplit__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_blocksplit__10" c_casadi_blocksplit__10
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_blocksplit__10
  :: IM -> Int -> Int -> IO (Vector (Vector IM))
casadi_blocksplit__10 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__10 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__11" c_casadi_blocksplit__11
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_blocksplit__11
  :: IM -> Vector Int -> Vector Int -> IO (Vector (Vector IM))
casadi_blocksplit__11 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__11 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__12" c_casadi_blocksplit__12
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_blocksplit__12
  :: MX -> IO (Vector (Vector MX))
casadi_blocksplit__12 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__12 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blocksplit__13" c_casadi_blocksplit__13
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_blocksplit__13
  :: MX -> Int -> IO (Vector (Vector MX))
casadi_blocksplit__13 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__13 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_blocksplit__14" c_casadi_blocksplit__14
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_blocksplit__14
  :: MX -> Int -> Int -> IO (Vector (Vector MX))
casadi_blocksplit__14 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__14 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__15" c_casadi_blocksplit__15
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_blocksplit__15
  :: MX -> Vector Int -> Vector Int -> IO (Vector (Vector MX))
casadi_blocksplit__15 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__15 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__16" c_casadi_blocksplit__16
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec (Ptr (StdVec (Ptr Sparsity')))))

casadi_blocksplit__16
  :: Sparsity -> IO (Vector (Vector Sparsity))
casadi_blocksplit__16 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__16 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_blocksplit__17" c_casadi_blocksplit__17
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr Sparsity')))))

casadi_blocksplit__17
  :: Sparsity -> Int -> IO (Vector (Vector Sparsity))
casadi_blocksplit__17 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__17 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_blocksplit__18" c_casadi_blocksplit__18
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr Sparsity')))))

casadi_blocksplit__18
  :: Sparsity -> Int -> Int -> IO (Vector (Vector Sparsity))
casadi_blocksplit__18 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__18 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_blocksplit__19" c_casadi_blocksplit__19
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr Sparsity')))))

casadi_blocksplit__19
  :: Sparsity -> Vector Int -> Vector Int -> IO (Vector (Vector Sparsity))
casadi_blocksplit__19 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_blocksplit__19 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_ceil__0" c_casadi_ceil__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_ceil__0
  :: Double -> IO Double
casadi_ceil__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ceil__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_ceil__1" c_casadi_ceil__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_ceil__1
  :: SX -> IO SX
casadi_ceil__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ceil__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_ceil__2" c_casadi_ceil__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_ceil__2
  :: DM -> IO DM
casadi_ceil__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ceil__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_ceil__3" c_casadi_ceil__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_ceil__3
  :: IM -> IO IM
casadi_ceil__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ceil__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_ceil__4" c_casadi_ceil__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_ceil__4
  :: MX -> IO MX
casadi_ceil__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ceil__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_chol__0" c_casadi_chol__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_chol__0
  :: SX -> IO SX
casadi_chol__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_chol__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_chol__1" c_casadi_chol__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_chol__1
  :: DM -> IO DM
casadi_chol__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_chol__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_chol__2" c_casadi_chol__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_chol__2
  :: IM -> IO IM
casadi_chol__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_chol__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cofactor__0" c_casadi_cofactor__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> CInt -> IO (Ptr SX')

casadi_cofactor__0
  :: SX -> Int -> Int -> IO SX
casadi_cofactor__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cofactor__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_cofactor__1" c_casadi_cofactor__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> CInt -> IO (Ptr DM')

casadi_cofactor__1
  :: DM -> Int -> Int -> IO DM
casadi_cofactor__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cofactor__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_cofactor__2" c_casadi_cofactor__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO (Ptr IM')

casadi_cofactor__2
  :: IM -> Int -> Int -> IO IM
casadi_cofactor__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cofactor__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_conditional__0" c_casadi_conditional__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec (Ptr SX')) -> Ptr SX' -> IO (Ptr SX')

casadi_conditional__0
  :: SX -> Vector SX -> SX -> IO SX
casadi_conditional__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_conditional__1" c_casadi_conditional__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec (Ptr SX')) -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_conditional__1
  :: SX -> Vector SX -> SX -> Bool -> IO SX
casadi_conditional__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_conditional__2" c_casadi_conditional__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec (Ptr DM')) -> Ptr DM' -> IO (Ptr DM')

casadi_conditional__2
  :: DM -> Vector DM -> DM -> IO DM
casadi_conditional__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_conditional__3" c_casadi_conditional__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec (Ptr DM')) -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_conditional__3
  :: DM -> Vector DM -> DM -> Bool -> IO DM
casadi_conditional__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_conditional__4" c_casadi_conditional__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec (Ptr IM')) -> Ptr IM' -> IO (Ptr IM')

casadi_conditional__4
  :: IM -> Vector IM -> IM -> IO IM
casadi_conditional__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_conditional__5" c_casadi_conditional__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec (Ptr IM')) -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_conditional__5
  :: IM -> Vector IM -> IM -> Bool -> IO IM
casadi_conditional__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_conditional__6" c_casadi_conditional__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> Ptr MX' -> IO (Ptr MX')

casadi_conditional__6
  :: MX -> Vector MX -> MX -> IO MX
casadi_conditional__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_conditional__7" c_casadi_conditional__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_conditional__7
  :: MX -> Vector MX -> MX -> Bool -> IO MX
casadi_conditional__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_conditional__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_constpow__0" c_casadi_constpow__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_constpow__0
  :: Double -> Double -> IO Double
casadi_constpow__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_constpow__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_constpow__1" c_casadi_constpow__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_constpow__1
  :: SX -> SX -> IO SX
casadi_constpow__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_constpow__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_constpow__2" c_casadi_constpow__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_constpow__2
  :: DM -> DM -> IO DM
casadi_constpow__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_constpow__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_constpow__3" c_casadi_constpow__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_constpow__3
  :: IM -> IM -> IO IM
casadi_constpow__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_constpow__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_constpow__4" c_casadi_constpow__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_constpow__4
  :: MX -> MX -> IO MX
casadi_constpow__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_constpow__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_copysign__0" c_casadi_copysign__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_copysign__0
  :: Double -> Double -> IO Double
casadi_copysign__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_copysign__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_copysign__1" c_casadi_copysign__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_copysign__1
  :: SX -> SX -> IO SX
casadi_copysign__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_copysign__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_copysign__2" c_casadi_copysign__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_copysign__2
  :: DM -> DM -> IO DM
casadi_copysign__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_copysign__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_copysign__3" c_casadi_copysign__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_copysign__3
  :: IM -> IM -> IO IM
casadi_copysign__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_copysign__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_copysign__4" c_casadi_copysign__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_copysign__4
  :: MX -> MX -> IO MX
casadi_copysign__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_copysign__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_cos__0" c_casadi_cos__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_cos__0
  :: Double -> IO Double
casadi_cos__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cos__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cos__1" c_casadi_cos__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_cos__1
  :: SX -> IO SX
casadi_cos__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cos__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cos__2" c_casadi_cos__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_cos__2
  :: DM -> IO DM
casadi_cos__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cos__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cos__3" c_casadi_cos__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_cos__3
  :: IM -> IO IM
casadi_cos__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cos__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cos__4" c_casadi_cos__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_cos__4
  :: MX -> IO MX
casadi_cos__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cos__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cosh__0" c_casadi_cosh__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_cosh__0
  :: Double -> IO Double
casadi_cosh__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cosh__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cosh__1" c_casadi_cosh__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_cosh__1
  :: SX -> IO SX
casadi_cosh__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cosh__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cosh__2" c_casadi_cosh__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_cosh__2
  :: DM -> IO DM
casadi_cosh__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cosh__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cosh__3" c_casadi_cosh__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_cosh__3
  :: IM -> IO IM
casadi_cosh__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cosh__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cosh__4" c_casadi_cosh__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_cosh__4
  :: MX -> IO MX
casadi_cosh__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cosh__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_cross__0" c_casadi_cross__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_cross__0
  :: SX -> SX -> IO SX
casadi_cross__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_cross__1" c_casadi_cross__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_cross__1
  :: SX -> SX -> Int -> IO SX
casadi_cross__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_cross__2" c_casadi_cross__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_cross__2
  :: DM -> DM -> IO DM
casadi_cross__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_cross__3" c_casadi_cross__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_cross__3
  :: DM -> DM -> Int -> IO DM
casadi_cross__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_cross__4" c_casadi_cross__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_cross__4
  :: IM -> IM -> IO IM
casadi_cross__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_cross__5" c_casadi_cross__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_cross__5
  :: IM -> IM -> Int -> IO IM
casadi_cross__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_cross__6" c_casadi_cross__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_cross__6
  :: MX -> MX -> IO MX
casadi_cross__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_cross__7" c_casadi_cross__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_cross__7
  :: MX -> MX -> Int -> IO MX
casadi_cross__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_cross__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_densify__0" c_casadi_densify__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_densify__0
  :: SX -> IO SX
casadi_densify__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_densify__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_densify__1" c_casadi_densify__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_densify__1
  :: DM -> IO DM
casadi_densify__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_densify__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_densify__2" c_casadi_densify__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_densify__2
  :: IM -> IO IM
casadi_densify__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_densify__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_densify__3" c_casadi_densify__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_densify__3
  :: MX -> IO MX
casadi_densify__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_densify__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_depends_on__0" c_casadi_depends_on__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO CInt

casadi_depends_on__0
  :: SX -> SX -> IO Bool
casadi_depends_on__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_depends_on__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_depends_on__1" c_casadi_depends_on__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO CInt

casadi_depends_on__1
  :: DM -> DM -> IO Bool
casadi_depends_on__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_depends_on__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_depends_on__2" c_casadi_depends_on__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO CInt

casadi_depends_on__2
  :: IM -> IM -> IO Bool
casadi_depends_on__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_depends_on__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_depends_on__3" c_casadi_depends_on__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO CInt

casadi_depends_on__3
  :: MX -> MX -> IO Bool
casadi_depends_on__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_depends_on__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_det__0" c_casadi_det__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_det__0
  :: SX -> IO SX
casadi_det__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_det__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_det__1" c_casadi_det__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_det__1
  :: DM -> IO DM
casadi_det__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_det__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_det__2" c_casadi_det__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_det__2
  :: IM -> IO IM
casadi_det__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_det__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_det__3" c_casadi_det__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_det__3
  :: MX -> IO MX
casadi_det__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_det__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diag__0" c_casadi_diag__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_diag__0
  :: SX -> IO SX
casadi_diag__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diag__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diag__1" c_casadi_diag__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_diag__1
  :: DM -> IO DM
casadi_diag__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diag__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diag__2" c_casadi_diag__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_diag__2
  :: IM -> IO IM
casadi_diag__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diag__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diag__3" c_casadi_diag__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_diag__3
  :: MX -> IO MX
casadi_diag__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diag__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagcat__0" c_casadi_diagcat__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr SX')

casadi_diagcat__0
  :: Vector SX -> IO SX
casadi_diagcat__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagcat__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagcat__1" c_casadi_diagcat__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> IO (Ptr DM')

casadi_diagcat__1
  :: Vector DM -> IO DM
casadi_diagcat__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagcat__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagcat__2" c_casadi_diagcat__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> IO (Ptr IM')

casadi_diagcat__2
  :: Vector IM -> IO IM
casadi_diagcat__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagcat__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagcat__3" c_casadi_diagcat__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi_diagcat__3
  :: Vector MX -> IO MX
casadi_diagcat__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagcat__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagcat__4" c_casadi_diagcat__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Sparsity')) -> IO (Ptr Sparsity')

casadi_diagcat__4
  :: Vector Sparsity -> IO Sparsity
casadi_diagcat__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagcat__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagsplit__0" c_casadi_diagsplit__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr SX')))

casadi_diagsplit__0
  :: SX -> Int -> Int -> IO (Vector SX)
casadi_diagsplit__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__1" c_casadi_diagsplit__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec (Ptr SX')))

casadi_diagsplit__1
  :: SX -> IO (Vector SX)
casadi_diagsplit__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagsplit__2" c_casadi_diagsplit__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr (StdVec (Ptr SX')))

casadi_diagsplit__2
  :: SX -> Int -> IO (Vector SX)
casadi_diagsplit__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__3" c_casadi_diagsplit__3
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr SX')))

casadi_diagsplit__3
  :: SX -> Vector Int -> IO (Vector SX)
casadi_diagsplit__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__4" c_casadi_diagsplit__4
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr SX')))

casadi_diagsplit__4
  :: SX -> Vector Int -> Vector Int -> IO (Vector SX)
casadi_diagsplit__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__5" c_casadi_diagsplit__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr DM')))

casadi_diagsplit__5
  :: DM -> Int -> Int -> IO (Vector DM)
casadi_diagsplit__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__6" c_casadi_diagsplit__6
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec (Ptr DM')))

casadi_diagsplit__6
  :: DM -> IO (Vector DM)
casadi_diagsplit__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagsplit__7" c_casadi_diagsplit__7
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr (StdVec (Ptr DM')))

casadi_diagsplit__7
  :: DM -> Int -> IO (Vector DM)
casadi_diagsplit__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__8" c_casadi_diagsplit__8
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr DM')))

casadi_diagsplit__8
  :: DM -> Vector Int -> IO (Vector DM)
casadi_diagsplit__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__9" c_casadi_diagsplit__9
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr DM')))

casadi_diagsplit__9
  :: DM -> Vector Int -> Vector Int -> IO (Vector DM)
casadi_diagsplit__9 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__9 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__10" c_casadi_diagsplit__10
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr IM')))

casadi_diagsplit__10
  :: IM -> Int -> Int -> IO (Vector IM)
casadi_diagsplit__10 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__10 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__11" c_casadi_diagsplit__11
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec (Ptr IM')))

casadi_diagsplit__11
  :: IM -> IO (Vector IM)
casadi_diagsplit__11 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__11 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagsplit__12" c_casadi_diagsplit__12
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr (StdVec (Ptr IM')))

casadi_diagsplit__12
  :: IM -> Int -> IO (Vector IM)
casadi_diagsplit__12 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__12 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__13" c_casadi_diagsplit__13
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr IM')))

casadi_diagsplit__13
  :: IM -> Vector Int -> IO (Vector IM)
casadi_diagsplit__13 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__13 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__14" c_casadi_diagsplit__14
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr IM')))

casadi_diagsplit__14
  :: IM -> Vector Int -> Vector Int -> IO (Vector IM)
casadi_diagsplit__14 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__14 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__15" c_casadi_diagsplit__15
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi_diagsplit__15
  :: MX -> Int -> Int -> IO (Vector MX)
casadi_diagsplit__15 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__15 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__16" c_casadi_diagsplit__16
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec (Ptr MX')))

casadi_diagsplit__16
  :: MX -> IO (Vector MX)
casadi_diagsplit__16 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__16 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagsplit__17" c_casadi_diagsplit__17
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi_diagsplit__17
  :: MX -> Int -> IO (Vector MX)
casadi_diagsplit__17 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__17 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__18" c_casadi_diagsplit__18
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr MX')))

casadi_diagsplit__18
  :: MX -> Vector Int -> IO (Vector MX)
casadi_diagsplit__18 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__18 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__19" c_casadi_diagsplit__19
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr MX')))

casadi_diagsplit__19
  :: MX -> Vector Int -> Vector Int -> IO (Vector MX)
casadi_diagsplit__19 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__19 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__20" c_casadi_diagsplit__20
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_diagsplit__20
  :: Sparsity -> Int -> Int -> IO (Vector Sparsity)
casadi_diagsplit__20 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__20 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_diagsplit__21" c_casadi_diagsplit__21
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_diagsplit__21
  :: Sparsity -> IO (Vector Sparsity)
casadi_diagsplit__21 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__21 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_diagsplit__22" c_casadi_diagsplit__22
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_diagsplit__22
  :: Sparsity -> Int -> IO (Vector Sparsity)
casadi_diagsplit__22 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__22 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__23" c_casadi_diagsplit__23
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_diagsplit__23
  :: Sparsity -> Vector Int -> IO (Vector Sparsity)
casadi_diagsplit__23 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__23 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_diagsplit__24" c_casadi_diagsplit__24
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_diagsplit__24
  :: Sparsity -> Vector Int -> Vector Int -> IO (Vector Sparsity)
casadi_diagsplit__24 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_diagsplit__24 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_dot__0" c_casadi_dot__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_dot__0
  :: SX -> SX -> IO SX
casadi_dot__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_dot__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_dot__1" c_casadi_dot__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_dot__1
  :: DM -> DM -> IO DM
casadi_dot__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_dot__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_dot__2" c_casadi_dot__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_dot__2
  :: IM -> IM -> IO IM
casadi_dot__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_dot__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_dot__3" c_casadi_dot__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_dot__3
  :: MX -> MX -> IO MX
casadi_dot__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_dot__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_eig_symbolic__0" c_casadi_eig_symbolic__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_eig_symbolic__0
  :: SX -> IO SX
casadi_eig_symbolic__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eig_symbolic__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_eig_symbolic__1" c_casadi_eig_symbolic__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_eig_symbolic__1
  :: DM -> IO DM
casadi_eig_symbolic__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eig_symbolic__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_eig_symbolic__2" c_casadi_eig_symbolic__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_eig_symbolic__2
  :: IM -> IO IM
casadi_eig_symbolic__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eig_symbolic__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_eq__0" c_casadi_eq__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_eq__0
  :: Double -> Double -> IO Double
casadi_eq__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eq__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_eq__1" c_casadi_eq__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_eq__1
  :: SX -> SX -> IO SX
casadi_eq__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eq__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_eq__2" c_casadi_eq__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_eq__2
  :: DM -> DM -> IO DM
casadi_eq__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eq__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_eq__3" c_casadi_eq__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_eq__3
  :: IM -> IM -> IO IM
casadi_eq__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eq__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_eq__4" c_casadi_eq__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_eq__4
  :: MX -> MX -> IO MX
casadi_eq__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_eq__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_erf__0" c_casadi_erf__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_erf__0
  :: Double -> IO Double
casadi_erf__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erf__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erf__1" c_casadi_erf__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_erf__1
  :: SX -> IO SX
casadi_erf__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erf__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erf__2" c_casadi_erf__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_erf__2
  :: DM -> IO DM
casadi_erf__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erf__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erf__3" c_casadi_erf__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_erf__3
  :: IM -> IO IM
casadi_erf__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erf__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erf__4" c_casadi_erf__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_erf__4
  :: MX -> IO MX
casadi_erf__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erf__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erfinv__0" c_casadi_erfinv__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_erfinv__0
  :: Double -> IO Double
casadi_erfinv__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erfinv__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erfinv__1" c_casadi_erfinv__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_erfinv__1
  :: SX -> IO SX
casadi_erfinv__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erfinv__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erfinv__2" c_casadi_erfinv__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_erfinv__2
  :: DM -> IO DM
casadi_erfinv__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erfinv__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erfinv__3" c_casadi_erfinv__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_erfinv__3
  :: IM -> IO IM
casadi_erfinv__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erfinv__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_erfinv__4" c_casadi_erfinv__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_erfinv__4
  :: MX -> IO MX
casadi_erfinv__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_erfinv__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_exp__0" c_casadi_exp__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_exp__0
  :: Double -> IO Double
casadi_exp__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_exp__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_exp__1" c_casadi_exp__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_exp__1
  :: SX -> IO SX
casadi_exp__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_exp__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_exp__2" c_casadi_exp__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_exp__2
  :: DM -> IO DM
casadi_exp__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_exp__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_exp__3" c_casadi_exp__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_exp__3
  :: IM -> IO IM
casadi_exp__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_exp__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_exp__4" c_casadi_exp__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_exp__4
  :: MX -> IO MX
casadi_exp__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_exp__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_expand__0" c_casadi_expand__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO ()

casadi_expand__0
  :: SX -> SX -> SX -> IO ()
casadi_expand__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_expand__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_expand__1" c_casadi_expand__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO ()

casadi_expand__1
  :: DM -> DM -> DM -> IO ()
casadi_expand__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_expand__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_expand__2" c_casadi_expand__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO ()

casadi_expand__2
  :: IM -> IM -> IM -> IO ()
casadi_expand__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_expand__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_find" c_casadi_find
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_find
  :: MX -> IO MX
casadi_find x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_find errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_floor__0" c_casadi_floor__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_floor__0
  :: Double -> IO Double
casadi_floor__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_floor__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_floor__1" c_casadi_floor__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_floor__1
  :: SX -> IO SX
casadi_floor__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_floor__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_floor__2" c_casadi_floor__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_floor__2
  :: DM -> IO DM
casadi_floor__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_floor__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_floor__3" c_casadi_floor__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_floor__3
  :: IM -> IO IM
casadi_floor__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_floor__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_floor__4" c_casadi_floor__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_floor__4
  :: MX -> IO MX
casadi_floor__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_floor__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_forward__0" c_casadi_forward__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr (StdVec (Ptr SX')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_forward__0
  :: Vector SX -> Vector SX -> Vector (Vector SX) -> IO (Vector (Vector SX))
casadi_forward__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_forward__1" c_casadi_forward__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr (StdVec (Ptr SX')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_forward__1
  :: Vector SX -> Vector SX -> Vector (Vector SX) -> M.Map String GenericType -> IO (Vector (Vector SX))
casadi_forward__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_forward__2" c_casadi_forward__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr (StdVec (Ptr DM')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_forward__2
  :: Vector DM -> Vector DM -> Vector (Vector DM) -> IO (Vector (Vector DM))
casadi_forward__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_forward__3" c_casadi_forward__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr (StdVec (Ptr DM')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_forward__3
  :: Vector DM -> Vector DM -> Vector (Vector DM) -> M.Map String GenericType -> IO (Vector (Vector DM))
casadi_forward__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_forward__4" c_casadi_forward__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr (StdVec (Ptr IM')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_forward__4
  :: Vector IM -> Vector IM -> Vector (Vector IM) -> IO (Vector (Vector IM))
casadi_forward__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_forward__5" c_casadi_forward__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr (StdVec (Ptr IM')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_forward__5
  :: Vector IM -> Vector IM -> Vector (Vector IM) -> M.Map String GenericType -> IO (Vector (Vector IM))
casadi_forward__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_forward__6" c_casadi_forward__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr (StdVec (Ptr MX')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_forward__6
  :: Vector MX -> Vector MX -> Vector (Vector MX) -> IO (Vector (Vector MX))
casadi_forward__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_forward__7" c_casadi_forward__7
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr (StdVec (Ptr MX')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_forward__7
  :: Vector MX -> Vector MX -> Vector (Vector MX) -> M.Map String GenericType -> IO (Vector (Vector MX))
casadi_forward__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_forward__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__0" c_casadi_gauss_quadrature__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> Ptr SX' -> IO (Ptr SX')

casadi_gauss_quadrature__0
  :: SX -> SX -> SX -> SX -> Int -> SX -> IO SX
casadi_gauss_quadrature__0 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__0 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__1" c_casadi_gauss_quadrature__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_gauss_quadrature__1
  :: SX -> SX -> SX -> SX -> IO SX
casadi_gauss_quadrature__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__2" c_casadi_gauss_quadrature__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_gauss_quadrature__2
  :: SX -> SX -> SX -> SX -> Int -> IO SX
casadi_gauss_quadrature__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__2 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__3" c_casadi_gauss_quadrature__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> Ptr DM' -> CInt -> Ptr DM' -> IO (Ptr DM')

casadi_gauss_quadrature__3
  :: DM -> DM -> DM -> DM -> Int -> DM -> IO DM
casadi_gauss_quadrature__3 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__3 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__4" c_casadi_gauss_quadrature__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_gauss_quadrature__4
  :: DM -> DM -> DM -> DM -> IO DM
casadi_gauss_quadrature__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__5" c_casadi_gauss_quadrature__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_gauss_quadrature__5
  :: DM -> DM -> DM -> DM -> Int -> IO DM
casadi_gauss_quadrature__5 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__5 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__6" c_casadi_gauss_quadrature__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> Ptr IM' -> CInt -> Ptr IM' -> IO (Ptr IM')

casadi_gauss_quadrature__6
  :: IM -> IM -> IM -> IM -> Int -> IM -> IO IM
casadi_gauss_quadrature__6 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__6 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__7" c_casadi_gauss_quadrature__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_gauss_quadrature__7
  :: IM -> IM -> IM -> IM -> IO IM
casadi_gauss_quadrature__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_gauss_quadrature__8" c_casadi_gauss_quadrature__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_gauss_quadrature__8
  :: IM -> IM -> IM -> IM -> Int -> IO IM
casadi_gauss_quadrature__8 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gauss_quadrature__8 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "casadi_ge__0" c_casadi_ge__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_ge__0
  :: Double -> Double -> IO Double
casadi_ge__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ge__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ge__1" c_casadi_ge__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_ge__1
  :: SX -> SX -> IO SX
casadi_ge__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ge__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ge__2" c_casadi_ge__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_ge__2
  :: DM -> DM -> IO DM
casadi_ge__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ge__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ge__3" c_casadi_ge__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_ge__3
  :: IM -> IM -> IO IM
casadi_ge__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ge__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ge__4" c_casadi_ge__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_ge__4
  :: MX -> MX -> IO MX
casadi_ge__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ge__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_getMinor__0" c_casadi_getMinor__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> CInt -> IO (Ptr SX')

casadi_getMinor__0
  :: SX -> Int -> Int -> IO SX
casadi_getMinor__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_getMinor__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_getMinor__1" c_casadi_getMinor__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> CInt -> IO (Ptr DM')

casadi_getMinor__1
  :: DM -> Int -> Int -> IO DM
casadi_getMinor__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_getMinor__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_getMinor__2" c_casadi_getMinor__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO (Ptr IM')

casadi_getMinor__2
  :: IM -> Int -> Int -> IO IM
casadi_getMinor__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_getMinor__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_gradient__0" c_casadi_gradient__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_gradient__0
  :: SX -> SX -> IO SX
casadi_gradient__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gradient__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_gradient__1" c_casadi_gradient__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_gradient__1
  :: DM -> DM -> IO DM
casadi_gradient__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gradient__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_gradient__2" c_casadi_gradient__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_gradient__2
  :: IM -> IM -> IO IM
casadi_gradient__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gradient__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_gradient__3" c_casadi_gradient__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_gradient__3
  :: MX -> MX -> IO MX
casadi_gradient__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gradient__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_graph_substitute__0" c_casadi_graph_substitute__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr (StdVec (Ptr MX')))

casadi_graph_substitute__0
  :: Vector MX -> Vector MX -> Vector MX -> IO (Vector MX)
casadi_graph_substitute__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_graph_substitute__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_graph_substitute__1" c_casadi_graph_substitute__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi_graph_substitute__1
  :: MX -> Vector MX -> Vector MX -> IO MX
casadi_graph_substitute__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_graph_substitute__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_gt__0" c_casadi_gt__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_gt__0
  :: Double -> Double -> IO Double
casadi_gt__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gt__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_gt__1" c_casadi_gt__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_gt__1
  :: SX -> SX -> IO SX
casadi_gt__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gt__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_gt__2" c_casadi_gt__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_gt__2
  :: DM -> DM -> IO DM
casadi_gt__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gt__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_gt__3" c_casadi_gt__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_gt__3
  :: IM -> IM -> IO IM
casadi_gt__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gt__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_gt__4" c_casadi_gt__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_gt__4
  :: MX -> MX -> IO MX
casadi_gt__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_gt__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_heaviside__0" c_casadi_heaviside__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_heaviside__0
  :: SX -> IO SX
casadi_heaviside__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_heaviside__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_heaviside__1" c_casadi_heaviside__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_heaviside__1
  :: DM -> IO DM
casadi_heaviside__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_heaviside__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_heaviside__2" c_casadi_heaviside__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_heaviside__2
  :: IM -> IO IM
casadi_heaviside__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_heaviside__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_hessian__0" c_casadi_hessian__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_hessian__0
  :: SX -> SX -> SX -> IO SX
casadi_hessian__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_hessian__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_hessian__1" c_casadi_hessian__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_hessian__1
  :: DM -> DM -> DM -> IO DM
casadi_hessian__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_hessian__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_hessian__2" c_casadi_hessian__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_hessian__2
  :: IM -> IM -> IM -> IO IM
casadi_hessian__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_hessian__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_hessian__3" c_casadi_hessian__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_hessian__3
  :: MX -> MX -> MX -> IO MX
casadi_hessian__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_hessian__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_horzcat__0" c_casadi_horzcat__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr SX')

casadi_horzcat__0
  :: Vector SX -> IO SX
casadi_horzcat__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzcat__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzcat__1" c_casadi_horzcat__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> IO (Ptr DM')

casadi_horzcat__1
  :: Vector DM -> IO DM
casadi_horzcat__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzcat__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzcat__2" c_casadi_horzcat__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> IO (Ptr IM')

casadi_horzcat__2
  :: Vector IM -> IO IM
casadi_horzcat__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzcat__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzcat__3" c_casadi_horzcat__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi_horzcat__3
  :: Vector MX -> IO MX
casadi_horzcat__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzcat__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzcat__4" c_casadi_horzcat__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Sparsity')) -> IO (Ptr Sparsity')

casadi_horzcat__4
  :: Vector Sparsity -> IO Sparsity
casadi_horzcat__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzcat__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzsplit__0" c_casadi_horzsplit__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec (Ptr SX')))

casadi_horzsplit__0
  :: SX -> IO (Vector SX)
casadi_horzsplit__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzsplit__1" c_casadi_horzsplit__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr (StdVec (Ptr SX')))

casadi_horzsplit__1
  :: SX -> Int -> IO (Vector SX)
casadi_horzsplit__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__2" c_casadi_horzsplit__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr SX')))

casadi_horzsplit__2
  :: SX -> Vector Int -> IO (Vector SX)
casadi_horzsplit__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__3" c_casadi_horzsplit__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec (Ptr DM')))

casadi_horzsplit__3
  :: DM -> IO (Vector DM)
casadi_horzsplit__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzsplit__4" c_casadi_horzsplit__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr (StdVec (Ptr DM')))

casadi_horzsplit__4
  :: DM -> Int -> IO (Vector DM)
casadi_horzsplit__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__5" c_casadi_horzsplit__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr DM')))

casadi_horzsplit__5
  :: DM -> Vector Int -> IO (Vector DM)
casadi_horzsplit__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__6" c_casadi_horzsplit__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec (Ptr IM')))

casadi_horzsplit__6
  :: IM -> IO (Vector IM)
casadi_horzsplit__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzsplit__7" c_casadi_horzsplit__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr (StdVec (Ptr IM')))

casadi_horzsplit__7
  :: IM -> Int -> IO (Vector IM)
casadi_horzsplit__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__8" c_casadi_horzsplit__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr IM')))

casadi_horzsplit__8
  :: IM -> Vector Int -> IO (Vector IM)
casadi_horzsplit__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__9" c_casadi_horzsplit__9
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec (Ptr MX')))

casadi_horzsplit__9
  :: MX -> IO (Vector MX)
casadi_horzsplit__9 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__9 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzsplit__10" c_casadi_horzsplit__10
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi_horzsplit__10
  :: MX -> Int -> IO (Vector MX)
casadi_horzsplit__10 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__10 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__11" c_casadi_horzsplit__11
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr MX')))

casadi_horzsplit__11
  :: MX -> Vector Int -> IO (Vector MX)
casadi_horzsplit__11 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__11 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__12" c_casadi_horzsplit__12
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_horzsplit__12
  :: Sparsity -> IO (Vector Sparsity)
casadi_horzsplit__12 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__12 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_horzsplit__13" c_casadi_horzsplit__13
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_horzsplit__13
  :: Sparsity -> Int -> IO (Vector Sparsity)
casadi_horzsplit__13 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__13 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_horzsplit__14" c_casadi_horzsplit__14
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_horzsplit__14
  :: Sparsity -> Vector Int -> IO (Vector Sparsity)
casadi_horzsplit__14 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_horzsplit__14 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_if_else__0" c_casadi_if_else__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_if_else__0
  :: SX -> SX -> SX -> IO SX
casadi_if_else__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_if_else__1" c_casadi_if_else__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_if_else__1
  :: SX -> SX -> SX -> Bool -> IO SX
casadi_if_else__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_if_else__2" c_casadi_if_else__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_if_else__2
  :: DM -> DM -> DM -> IO DM
casadi_if_else__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_if_else__3" c_casadi_if_else__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_if_else__3
  :: DM -> DM -> DM -> Bool -> IO DM
casadi_if_else__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_if_else__4" c_casadi_if_else__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_if_else__4
  :: IM -> IM -> IM -> IO IM
casadi_if_else__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_if_else__5" c_casadi_if_else__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_if_else__5
  :: IM -> IM -> IM -> Bool -> IO IM
casadi_if_else__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_if_else__6" c_casadi_if_else__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_if_else__6
  :: MX -> MX -> MX -> IO MX
casadi_if_else__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_if_else__7" c_casadi_if_else__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_if_else__7
  :: MX -> MX -> MX -> Bool -> IO MX
casadi_if_else__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_if_else__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_inv__0" c_casadi_inv__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_inv__0
  :: SX -> IO SX
casadi_inv__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_inv__1" c_casadi_inv__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_inv__1
  :: DM -> IO DM
casadi_inv__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_inv__2" c_casadi_inv__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_inv__2
  :: IM -> IO IM
casadi_inv__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_inv__3" c_casadi_inv__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_inv__3
  :: MX -> IO MX
casadi_inv__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_inv_skew__0" c_casadi_inv_skew__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_inv_skew__0
  :: SX -> IO SX
casadi_inv_skew__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv_skew__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_inv_skew__1" c_casadi_inv_skew__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_inv_skew__1
  :: DM -> IO DM
casadi_inv_skew__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv_skew__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_inv_skew__2" c_casadi_inv_skew__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_inv_skew__2
  :: IM -> IO IM
casadi_inv_skew__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv_skew__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_inv_skew__3" c_casadi_inv_skew__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_inv_skew__3
  :: MX -> IO MX
casadi_inv_skew__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_inv_skew__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_is_equal__0" c_casadi_is_equal__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CInt

casadi_is_equal__0
  :: Double -> Double -> IO Bool
casadi_is_equal__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_is_equal__1" c_casadi_is_equal__1
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> CInt -> IO CInt

casadi_is_equal__1
  :: Double -> Double -> Int -> IO Bool
casadi_is_equal__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_is_equal__2" c_casadi_is_equal__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO CInt

casadi_is_equal__2
  :: SX -> SX -> IO Bool
casadi_is_equal__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_is_equal__3" c_casadi_is_equal__3
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> IO CInt

casadi_is_equal__3
  :: SX -> SX -> Int -> IO Bool
casadi_is_equal__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_is_equal__4" c_casadi_is_equal__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO CInt

casadi_is_equal__4
  :: DM -> DM -> IO Bool
casadi_is_equal__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_is_equal__5" c_casadi_is_equal__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> IO CInt

casadi_is_equal__5
  :: DM -> DM -> Int -> IO Bool
casadi_is_equal__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_is_equal__6" c_casadi_is_equal__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO CInt

casadi_is_equal__6
  :: IM -> IM -> IO Bool
casadi_is_equal__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_is_equal__7" c_casadi_is_equal__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> IO CInt

casadi_is_equal__7
  :: IM -> IM -> Int -> IO Bool
casadi_is_equal__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_is_equal__8" c_casadi_is_equal__8
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO CInt

casadi_is_equal__8
  :: MX -> MX -> IO Bool
casadi_is_equal__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_is_equal__9" c_casadi_is_equal__9
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> IO CInt

casadi_is_equal__9
  :: MX -> MX -> Int -> IO Bool
casadi_is_equal__9 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_is_equal__9 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_jacobian__0" c_casadi_jacobian__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_jacobian__0
  :: SX -> SX -> IO SX
casadi_jacobian__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jacobian__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_jacobian__1" c_casadi_jacobian__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_jacobian__1
  :: DM -> DM -> IO DM
casadi_jacobian__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jacobian__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_jacobian__2" c_casadi_jacobian__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_jacobian__2
  :: IM -> IM -> IO IM
casadi_jacobian__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jacobian__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_jacobian__3" c_casadi_jacobian__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_jacobian__3
  :: MX -> MX -> IO MX
casadi_jacobian__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jacobian__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_jtimes__0" c_casadi_jtimes__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_jtimes__0
  :: SX -> SX -> SX -> IO SX
casadi_jtimes__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_jtimes__1" c_casadi_jtimes__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_jtimes__1
  :: SX -> SX -> SX -> Bool -> IO SX
casadi_jtimes__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_jtimes__2" c_casadi_jtimes__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_jtimes__2
  :: DM -> DM -> DM -> IO DM
casadi_jtimes__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_jtimes__3" c_casadi_jtimes__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_jtimes__3
  :: DM -> DM -> DM -> Bool -> IO DM
casadi_jtimes__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_jtimes__4" c_casadi_jtimes__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_jtimes__4
  :: IM -> IM -> IM -> IO IM
casadi_jtimes__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_jtimes__5" c_casadi_jtimes__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_jtimes__5
  :: IM -> IM -> IM -> Bool -> IO IM
casadi_jtimes__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_jtimes__6" c_casadi_jtimes__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_jtimes__6
  :: MX -> MX -> MX -> IO MX
casadi_jtimes__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_jtimes__7" c_casadi_jtimes__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_jtimes__7
  :: MX -> MX -> MX -> Bool -> IO MX
casadi_jtimes__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_jtimes__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_kron__0" c_casadi_kron__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_kron__0
  :: SX -> SX -> IO SX
casadi_kron__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_kron__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_kron__1" c_casadi_kron__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_kron__1
  :: DM -> DM -> IO DM
casadi_kron__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_kron__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_kron__2" c_casadi_kron__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_kron__2
  :: IM -> IM -> IO IM
casadi_kron__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_kron__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_kron__3" c_casadi_kron__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_kron__3
  :: MX -> MX -> IO MX
casadi_kron__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_kron__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_kron__4" c_casadi_kron__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_kron__4
  :: Sparsity -> Sparsity -> IO Sparsity
casadi_kron__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_kron__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ldivide__0" c_casadi_ldivide__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_ldivide__0
  :: Double -> Double -> IO Double
casadi_ldivide__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ldivide__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ldivide__1" c_casadi_ldivide__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_ldivide__1
  :: SX -> SX -> IO SX
casadi_ldivide__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ldivide__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ldivide__2" c_casadi_ldivide__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_ldivide__2
  :: DM -> DM -> IO DM
casadi_ldivide__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ldivide__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ldivide__3" c_casadi_ldivide__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_ldivide__3
  :: IM -> IM -> IO IM
casadi_ldivide__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ldivide__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ldivide__4" c_casadi_ldivide__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_ldivide__4
  :: MX -> MX -> IO MX
casadi_ldivide__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ldivide__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_le__0" c_casadi_le__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_le__0
  :: Double -> Double -> IO Double
casadi_le__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_le__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_le__1" c_casadi_le__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_le__1
  :: SX -> SX -> IO SX
casadi_le__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_le__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_le__2" c_casadi_le__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_le__2
  :: DM -> DM -> IO DM
casadi_le__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_le__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_le__3" c_casadi_le__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_le__3
  :: IM -> IM -> IO IM
casadi_le__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_le__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_le__4" c_casadi_le__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_le__4
  :: MX -> MX -> IO MX
casadi_le__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_le__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_linspace__0" c_casadi_linspace__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_linspace__0
  :: SX -> SX -> Int -> IO SX
casadi_linspace__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_linspace__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_linspace__1" c_casadi_linspace__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_linspace__1
  :: DM -> DM -> Int -> IO DM
casadi_linspace__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_linspace__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_linspace__2" c_casadi_linspace__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_linspace__2
  :: IM -> IM -> Int -> IO IM
casadi_linspace__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_linspace__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_linspace__3" c_casadi_linspace__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_linspace__3
  :: MX -> MX -> Int -> IO MX
casadi_linspace__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_linspace__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_log__0" c_casadi_log__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_log__0
  :: Double -> IO Double
casadi_log__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log__1" c_casadi_log__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_log__1
  :: SX -> IO SX
casadi_log__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log__2" c_casadi_log__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_log__2
  :: DM -> IO DM
casadi_log__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log__3" c_casadi_log__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_log__3
  :: IM -> IO IM
casadi_log__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log__4" c_casadi_log__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_log__4
  :: MX -> IO MX
casadi_log__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log10__0" c_casadi_log10__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_log10__0
  :: Double -> IO Double
casadi_log10__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log10__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log10__1" c_casadi_log10__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_log10__1
  :: SX -> IO SX
casadi_log10__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log10__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log10__2" c_casadi_log10__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_log10__2
  :: DM -> IO DM
casadi_log10__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log10__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log10__3" c_casadi_log10__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_log10__3
  :: IM -> IO IM
casadi_log10__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log10__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_log10__4" c_casadi_log10__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_log10__4
  :: MX -> IO MX
casadi_log10__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_log10__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_lt__0" c_casadi_lt__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_lt__0
  :: Double -> Double -> IO Double
casadi_lt__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_lt__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_lt__1" c_casadi_lt__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_lt__1
  :: SX -> SX -> IO SX
casadi_lt__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_lt__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_lt__2" c_casadi_lt__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_lt__2
  :: DM -> DM -> IO DM
casadi_lt__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_lt__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_lt__3" c_casadi_lt__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_lt__3
  :: IM -> IM -> IO IM
casadi_lt__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_lt__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_lt__4" c_casadi_lt__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_lt__4
  :: MX -> MX -> IO MX
casadi_lt__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_lt__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mac__0" c_casadi_mac__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_mac__0
  :: SX -> SX -> SX -> IO SX
casadi_mac__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mac__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_mac__1" c_casadi_mac__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_mac__1
  :: DM -> DM -> DM -> IO DM
casadi_mac__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mac__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_mac__2" c_casadi_mac__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_mac__2
  :: IM -> IM -> IM -> IO IM
casadi_mac__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mac__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_mac__3" c_casadi_mac__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_mac__3
  :: MX -> MX -> MX -> IO MX
casadi_mac__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mac__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_mac__4" c_casadi_mac__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_mac__4
  :: Sparsity -> Sparsity -> Sparsity -> IO Sparsity
casadi_mac__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mac__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_matrix_expand__0" c_casadi_matrix_expand__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_matrix_expand__0
  :: MX -> IO MX
casadi_matrix_expand__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_matrix_expand__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_matrix_expand__1" c_casadi_matrix_expand__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi_matrix_expand__1
  :: MX -> Vector MX -> IO MX
casadi_matrix_expand__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_matrix_expand__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_matrix_expand__2" c_casadi_matrix_expand__2
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr MX')

casadi_matrix_expand__2
  :: MX -> Vector MX -> M.Map String GenericType -> IO MX
casadi_matrix_expand__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_matrix_expand__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_matrix_expand__3" c_casadi_matrix_expand__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr (StdVec (Ptr MX')))

casadi_matrix_expand__3
  :: Vector MX -> IO (Vector MX)
casadi_matrix_expand__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_matrix_expand__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_matrix_expand__4" c_casadi_matrix_expand__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr (StdVec (Ptr MX')))

casadi_matrix_expand__4
  :: Vector MX -> Vector MX -> IO (Vector MX)
casadi_matrix_expand__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_matrix_expand__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_matrix_expand__5" c_casadi_matrix_expand__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr MX')))

casadi_matrix_expand__5
  :: Vector MX -> Vector MX -> M.Map String GenericType -> IO (Vector MX)
casadi_matrix_expand__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_matrix_expand__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_max__0" c_casadi_max__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_max__0
  :: Double -> Double -> IO Double
casadi_max__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_max__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_max__1" c_casadi_max__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_max__1
  :: SX -> SX -> IO SX
casadi_max__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_max__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_max__2" c_casadi_max__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_max__2
  :: DM -> DM -> IO DM
casadi_max__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_max__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_max__3" c_casadi_max__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_max__3
  :: IM -> IM -> IO IM
casadi_max__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_max__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_max__4" c_casadi_max__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_max__4
  :: MX -> MX -> IO MX
casadi_max__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_max__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_min__0" c_casadi_min__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_min__0
  :: Double -> Double -> IO Double
casadi_min__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_min__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_min__1" c_casadi_min__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_min__1
  :: SX -> SX -> IO SX
casadi_min__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_min__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_min__2" c_casadi_min__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_min__2
  :: DM -> DM -> IO DM
casadi_min__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_min__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_min__3" c_casadi_min__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_min__3
  :: IM -> IM -> IO IM
casadi_min__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_min__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_min__4" c_casadi_min__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_min__4
  :: MX -> MX -> IO MX
casadi_min__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_min__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_minus__0" c_casadi_minus__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_minus__0
  :: Double -> Double -> IO Double
casadi_minus__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_minus__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_minus__1" c_casadi_minus__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_minus__1
  :: SX -> SX -> IO SX
casadi_minus__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_minus__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_minus__2" c_casadi_minus__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_minus__2
  :: DM -> DM -> IO DM
casadi_minus__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_minus__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_minus__3" c_casadi_minus__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_minus__3
  :: IM -> IM -> IO IM
casadi_minus__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_minus__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_minus__4" c_casadi_minus__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_minus__4
  :: MX -> MX -> IO MX
casadi_minus__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_minus__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mldivide__0" c_casadi_mldivide__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_mldivide__0
  :: SX -> SX -> IO SX
casadi_mldivide__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mldivide__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mldivide__1" c_casadi_mldivide__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_mldivide__1
  :: DM -> DM -> IO DM
casadi_mldivide__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mldivide__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mldivide__2" c_casadi_mldivide__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_mldivide__2
  :: IM -> IM -> IO IM
casadi_mldivide__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mldivide__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mldivide__3" c_casadi_mldivide__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_mldivide__3
  :: MX -> MX -> IO MX
casadi_mldivide__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mldivide__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mod__0" c_casadi_mod__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_mod__0
  :: Double -> Double -> IO Double
casadi_mod__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mod__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mod__1" c_casadi_mod__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_mod__1
  :: SX -> SX -> IO SX
casadi_mod__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mod__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mod__2" c_casadi_mod__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_mod__2
  :: DM -> DM -> IO DM
casadi_mod__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mod__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mod__3" c_casadi_mod__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_mod__3
  :: IM -> IM -> IO IM
casadi_mod__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mod__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mod__4" c_casadi_mod__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_mod__4
  :: MX -> MX -> IO MX
casadi_mod__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mod__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mpower__0" c_casadi_mpower__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_mpower__0
  :: SX -> SX -> IO SX
casadi_mpower__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mpower__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mpower__1" c_casadi_mpower__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_mpower__1
  :: DM -> DM -> IO DM
casadi_mpower__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mpower__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mpower__2" c_casadi_mpower__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_mpower__2
  :: IM -> IM -> IO IM
casadi_mpower__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mpower__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mpower__3" c_casadi_mpower__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_mpower__3
  :: MX -> MX -> IO MX
casadi_mpower__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mpower__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mrdivide__0" c_casadi_mrdivide__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_mrdivide__0
  :: SX -> SX -> IO SX
casadi_mrdivide__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mrdivide__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mrdivide__1" c_casadi_mrdivide__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_mrdivide__1
  :: DM -> DM -> IO DM
casadi_mrdivide__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mrdivide__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mrdivide__2" c_casadi_mrdivide__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_mrdivide__2
  :: IM -> IM -> IO IM
casadi_mrdivide__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mrdivide__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mrdivide__3" c_casadi_mrdivide__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_mrdivide__3
  :: MX -> MX -> IO MX
casadi_mrdivide__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mrdivide__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mtaylor__0" c_casadi_mtaylor__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> Ptr (StdVec CInt) -> IO (Ptr SX')

casadi_mtaylor__0
  :: SX -> SX -> SX -> Int -> Vector Int -> IO SX
casadi_mtaylor__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__0 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "casadi_mtaylor__1" c_casadi_mtaylor__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_mtaylor__1
  :: SX -> SX -> SX -> IO SX
casadi_mtaylor__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_mtaylor__2" c_casadi_mtaylor__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_mtaylor__2
  :: SX -> SX -> SX -> Int -> IO SX
casadi_mtaylor__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_mtaylor__3" c_casadi_mtaylor__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> CInt -> Ptr (StdVec CInt) -> IO (Ptr DM')

casadi_mtaylor__3
  :: DM -> DM -> DM -> Int -> Vector Int -> IO DM
casadi_mtaylor__3 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__3 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "casadi_mtaylor__4" c_casadi_mtaylor__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_mtaylor__4
  :: DM -> DM -> DM -> IO DM
casadi_mtaylor__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_mtaylor__5" c_casadi_mtaylor__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_mtaylor__5
  :: DM -> DM -> DM -> Int -> IO DM
casadi_mtaylor__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_mtaylor__6" c_casadi_mtaylor__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> CInt -> Ptr (StdVec CInt) -> IO (Ptr IM')

casadi_mtaylor__6
  :: IM -> IM -> IM -> Int -> Vector Int -> IO IM
casadi_mtaylor__6 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__6 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "casadi_mtaylor__7" c_casadi_mtaylor__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_mtaylor__7
  :: IM -> IM -> IM -> IO IM
casadi_mtaylor__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_mtaylor__8" c_casadi_mtaylor__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_mtaylor__8
  :: IM -> IM -> IM -> Int -> IO IM
casadi_mtaylor__8 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtaylor__8 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_mtimes__0" c_casadi_mtimes__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr SX')

casadi_mtimes__0
  :: Vector SX -> IO SX
casadi_mtimes__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_mtimes__1" c_casadi_mtimes__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_mtimes__1
  :: SX -> SX -> IO SX
casadi_mtimes__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mtimes__2" c_casadi_mtimes__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> IO (Ptr DM')

casadi_mtimes__2
  :: Vector DM -> IO DM
casadi_mtimes__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_mtimes__3" c_casadi_mtimes__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_mtimes__3
  :: DM -> DM -> IO DM
casadi_mtimes__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mtimes__4" c_casadi_mtimes__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> IO (Ptr IM')

casadi_mtimes__4
  :: Vector IM -> IO IM
casadi_mtimes__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_mtimes__5" c_casadi_mtimes__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_mtimes__5
  :: IM -> IM -> IO IM
casadi_mtimes__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mtimes__6" c_casadi_mtimes__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi_mtimes__6
  :: Vector MX -> IO MX
casadi_mtimes__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_mtimes__7" c_casadi_mtimes__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_mtimes__7
  :: MX -> MX -> IO MX
casadi_mtimes__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_mtimes__8" c_casadi_mtimes__8
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Sparsity')) -> IO (Ptr Sparsity')

casadi_mtimes__8
  :: Vector Sparsity -> IO Sparsity
casadi_mtimes__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_mtimes__9" c_casadi_mtimes__9
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_mtimes__9
  :: Sparsity -> Sparsity -> IO Sparsity
casadi_mtimes__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_mtimes__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_n_nodes__0" c_casadi_n_nodes__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi_n_nodes__0
  :: SX -> IO Int
casadi_n_nodes__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_n_nodes__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_n_nodes__1" c_casadi_n_nodes__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi_n_nodes__1
  :: DM -> IO Int
casadi_n_nodes__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_n_nodes__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_n_nodes__2" c_casadi_n_nodes__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi_n_nodes__2
  :: IM -> IO Int
casadi_n_nodes__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_n_nodes__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_n_nodes__3" c_casadi_n_nodes__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi_n_nodes__3
  :: MX -> IO Int
casadi_n_nodes__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_n_nodes__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_ne__0" c_casadi_ne__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_ne__0
  :: Double -> Double -> IO Double
casadi_ne__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ne__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ne__1" c_casadi_ne__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_ne__1
  :: SX -> SX -> IO SX
casadi_ne__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ne__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ne__2" c_casadi_ne__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_ne__2
  :: DM -> DM -> IO DM
casadi_ne__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ne__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ne__3" c_casadi_ne__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_ne__3
  :: IM -> IM -> IO IM
casadi_ne__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ne__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_ne__4" c_casadi_ne__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_ne__4
  :: MX -> MX -> IO MX
casadi_ne__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ne__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_0_mul__0" c_casadi_norm_0_mul__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO CInt

casadi_norm_0_mul__0
  :: SX -> SX -> IO Int
casadi_norm_0_mul__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_0_mul__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_0_mul__1" c_casadi_norm_0_mul__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO CInt

casadi_norm_0_mul__1
  :: DM -> DM -> IO Int
casadi_norm_0_mul__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_0_mul__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_0_mul__2" c_casadi_norm_0_mul__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO CInt

casadi_norm_0_mul__2
  :: IM -> IM -> IO Int
casadi_norm_0_mul__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_0_mul__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_0_mul__3" c_casadi_norm_0_mul__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO CInt

casadi_norm_0_mul__3
  :: MX -> MX -> IO Int
casadi_norm_0_mul__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_0_mul__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_0_mul__4" c_casadi_norm_0_mul__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO CInt

casadi_norm_0_mul__4
  :: Sparsity -> Sparsity -> IO Int
casadi_norm_0_mul__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_0_mul__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_1__0" c_casadi_norm_1__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_norm_1__0
  :: SX -> IO SX
casadi_norm_1__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_1__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_1__1" c_casadi_norm_1__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_norm_1__1
  :: DM -> IO DM
casadi_norm_1__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_1__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_1__2" c_casadi_norm_1__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_norm_1__2
  :: IM -> IO IM
casadi_norm_1__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_1__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_1__3" c_casadi_norm_1__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_norm_1__3
  :: MX -> IO MX
casadi_norm_1__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_1__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_2__0" c_casadi_norm_2__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_norm_2__0
  :: SX -> IO SX
casadi_norm_2__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_2__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_2__1" c_casadi_norm_2__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_norm_2__1
  :: DM -> IO DM
casadi_norm_2__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_2__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_2__2" c_casadi_norm_2__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_norm_2__2
  :: IM -> IO IM
casadi_norm_2__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_2__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_2__3" c_casadi_norm_2__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_norm_2__3
  :: MX -> IO MX
casadi_norm_2__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_2__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_fro__0" c_casadi_norm_fro__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_norm_fro__0
  :: SX -> IO SX
casadi_norm_fro__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_fro__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_fro__1" c_casadi_norm_fro__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_norm_fro__1
  :: DM -> IO DM
casadi_norm_fro__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_fro__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_fro__2" c_casadi_norm_fro__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_norm_fro__2
  :: IM -> IO IM
casadi_norm_fro__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_fro__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_fro__3" c_casadi_norm_fro__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_norm_fro__3
  :: MX -> IO MX
casadi_norm_fro__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_fro__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_inf__0" c_casadi_norm_inf__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_norm_inf__0
  :: SX -> IO SX
casadi_norm_inf__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_inf__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_inf__1" c_casadi_norm_inf__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_norm_inf__1
  :: DM -> IO DM
casadi_norm_inf__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_inf__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_inf__2" c_casadi_norm_inf__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_norm_inf__2
  :: IM -> IO IM
casadi_norm_inf__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_inf__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_inf__3" c_casadi_norm_inf__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_norm_inf__3
  :: MX -> IO MX
casadi_norm_inf__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_inf__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_norm_inf_mul__0" c_casadi_norm_inf_mul__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_norm_inf_mul__0
  :: SX -> SX -> IO SX
casadi_norm_inf_mul__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_inf_mul__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_inf_mul__1" c_casadi_norm_inf_mul__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_norm_inf_mul__1
  :: DM -> DM -> IO DM
casadi_norm_inf_mul__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_inf_mul__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_norm_inf_mul__2" c_casadi_norm_inf_mul__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_norm_inf_mul__2
  :: IM -> IM -> IO IM
casadi_norm_inf_mul__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_norm_inf_mul__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_not__0" c_casadi_not__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_not__0
  :: Double -> IO Double
casadi_not__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_not__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_not__1" c_casadi_not__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_not__1
  :: SX -> IO SX
casadi_not__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_not__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_not__2" c_casadi_not__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_not__2
  :: DM -> IO DM
casadi_not__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_not__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_not__3" c_casadi_not__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_not__3
  :: IM -> IO IM
casadi_not__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_not__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_not__4" c_casadi_not__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_not__4
  :: MX -> IO MX
casadi_not__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_not__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_nullspace__0" c_casadi_nullspace__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_nullspace__0
  :: SX -> IO SX
casadi_nullspace__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_nullspace__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_nullspace__1" c_casadi_nullspace__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_nullspace__1
  :: DM -> IO DM
casadi_nullspace__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_nullspace__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_nullspace__2" c_casadi_nullspace__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_nullspace__2
  :: IM -> IO IM
casadi_nullspace__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_nullspace__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_nullspace__3" c_casadi_nullspace__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_nullspace__3
  :: MX -> IO MX
casadi_nullspace__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_nullspace__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_offset__0" c_casadi_offset__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr (StdVec CInt))

casadi_offset__0
  :: Vector SX -> IO (Vector Int)
casadi_offset__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_offset__1" c_casadi_offset__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> CInt -> IO (Ptr (StdVec CInt))

casadi_offset__1
  :: Vector SX -> Bool -> IO (Vector Int)
casadi_offset__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_offset__2" c_casadi_offset__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> IO (Ptr (StdVec CInt))

casadi_offset__2
  :: Vector DM -> IO (Vector Int)
casadi_offset__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_offset__3" c_casadi_offset__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> CInt -> IO (Ptr (StdVec CInt))

casadi_offset__3
  :: Vector DM -> Bool -> IO (Vector Int)
casadi_offset__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_offset__4" c_casadi_offset__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> IO (Ptr (StdVec CInt))

casadi_offset__4
  :: Vector IM -> IO (Vector Int)
casadi_offset__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_offset__5" c_casadi_offset__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> CInt -> IO (Ptr (StdVec CInt))

casadi_offset__5
  :: Vector IM -> Bool -> IO (Vector Int)
casadi_offset__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_offset__6" c_casadi_offset__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr (StdVec CInt))

casadi_offset__6
  :: Vector MX -> IO (Vector Int)
casadi_offset__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_offset__7" c_casadi_offset__7
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> CInt -> IO (Ptr (StdVec CInt))

casadi_offset__7
  :: Vector MX -> Bool -> IO (Vector Int)
casadi_offset__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_offset__8" c_casadi_offset__8
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Sparsity')) -> IO (Ptr (StdVec CInt))

casadi_offset__8
  :: Vector Sparsity -> IO (Vector Int)
casadi_offset__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_offset__9" c_casadi_offset__9
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Sparsity')) -> CInt -> IO (Ptr (StdVec CInt))

casadi_offset__9
  :: Vector Sparsity -> Bool -> IO (Vector Int)
casadi_offset__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_offset__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_or__0" c_casadi_or__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_or__0
  :: Double -> Double -> IO Double
casadi_or__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_or__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_or__1" c_casadi_or__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_or__1
  :: SX -> SX -> IO SX
casadi_or__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_or__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_or__2" c_casadi_or__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_or__2
  :: DM -> DM -> IO DM
casadi_or__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_or__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_or__3" c_casadi_or__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_or__3
  :: IM -> IM -> IO IM
casadi_or__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_or__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_or__4" c_casadi_or__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_or__4
  :: MX -> MX -> IO MX
casadi_or__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_or__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_pinv__0" c_casadi_pinv__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr StdString -> IO (Ptr SX')

casadi_pinv__0
  :: SX -> String -> IO SX
casadi_pinv__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_pinv__1" c_casadi_pinv__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr SX')

casadi_pinv__1
  :: SX -> String -> M.Map String GenericType -> IO SX
casadi_pinv__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pinv__2" c_casadi_pinv__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_pinv__2
  :: SX -> IO SX
casadi_pinv__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_pinv__3" c_casadi_pinv__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr StdString -> IO (Ptr DM')

casadi_pinv__3
  :: DM -> String -> IO DM
casadi_pinv__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_pinv__4" c_casadi_pinv__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr DM')

casadi_pinv__4
  :: DM -> String -> M.Map String GenericType -> IO DM
casadi_pinv__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pinv__5" c_casadi_pinv__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_pinv__5
  :: DM -> IO DM
casadi_pinv__5 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__5 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_pinv__6" c_casadi_pinv__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr StdString -> IO (Ptr IM')

casadi_pinv__6
  :: IM -> String -> IO IM
casadi_pinv__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_pinv__7" c_casadi_pinv__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr IM')

casadi_pinv__7
  :: IM -> String -> M.Map String GenericType -> IO IM
casadi_pinv__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pinv__8" c_casadi_pinv__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_pinv__8
  :: IM -> IO IM
casadi_pinv__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_pinv__9" c_casadi_pinv__9
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr StdString -> IO (Ptr MX')

casadi_pinv__9
  :: MX -> String -> IO MX
casadi_pinv__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_pinv__10" c_casadi_pinv__10
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr MX')

casadi_pinv__10
  :: MX -> String -> M.Map String GenericType -> IO MX
casadi_pinv__10 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__10 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pinv__11" c_casadi_pinv__11
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_pinv__11
  :: MX -> IO MX
casadi_pinv__11 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pinv__11 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_plus__0" c_casadi_plus__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_plus__0
  :: Double -> Double -> IO Double
casadi_plus__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_plus__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_plus__1" c_casadi_plus__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_plus__1
  :: SX -> SX -> IO SX
casadi_plus__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_plus__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_plus__2" c_casadi_plus__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_plus__2
  :: DM -> DM -> IO DM
casadi_plus__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_plus__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_plus__3" c_casadi_plus__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_plus__3
  :: IM -> IM -> IO IM
casadi_plus__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_plus__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_plus__4" c_casadi_plus__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_plus__4
  :: MX -> MX -> IO MX
casadi_plus__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_plus__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_poly_coeff__0" c_casadi_poly_coeff__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_poly_coeff__0
  :: SX -> SX -> IO SX
casadi_poly_coeff__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_poly_coeff__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_poly_coeff__1" c_casadi_poly_coeff__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_poly_coeff__1
  :: DM -> DM -> IO DM
casadi_poly_coeff__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_poly_coeff__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_poly_coeff__2" c_casadi_poly_coeff__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_poly_coeff__2
  :: IM -> IM -> IO IM
casadi_poly_coeff__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_poly_coeff__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_poly_roots__0" c_casadi_poly_roots__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_poly_roots__0
  :: SX -> IO SX
casadi_poly_roots__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_poly_roots__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_poly_roots__1" c_casadi_poly_roots__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_poly_roots__1
  :: DM -> IO DM
casadi_poly_roots__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_poly_roots__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_poly_roots__2" c_casadi_poly_roots__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_poly_roots__2
  :: IM -> IO IM
casadi_poly_roots__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_poly_roots__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_polyval__0" c_casadi_polyval__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_polyval__0
  :: SX -> SX -> IO SX
casadi_polyval__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_polyval__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_polyval__1" c_casadi_polyval__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_polyval__1
  :: DM -> DM -> IO DM
casadi_polyval__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_polyval__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_polyval__2" c_casadi_polyval__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_polyval__2
  :: IM -> IM -> IO IM
casadi_polyval__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_polyval__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_polyval__3" c_casadi_polyval__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_polyval__3
  :: MX -> MX -> IO MX
casadi_polyval__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_polyval__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_power__0" c_casadi_power__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_power__0
  :: Double -> Double -> IO Double
casadi_power__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_power__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_power__1" c_casadi_power__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_power__1
  :: SX -> SX -> IO SX
casadi_power__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_power__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_power__2" c_casadi_power__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_power__2
  :: DM -> DM -> IO DM
casadi_power__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_power__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_power__3" c_casadi_power__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_power__3
  :: IM -> IM -> IO IM
casadi_power__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_power__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_power__4" c_casadi_power__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_power__4
  :: MX -> MX -> IO MX
casadi_power__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_power__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_print_operator__0" c_casadi_print_operator__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr StdString)

casadi_print_operator__0
  :: SX -> Vector String -> IO String
casadi_print_operator__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_print_operator__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_print_operator__1" c_casadi_print_operator__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr StdString)

casadi_print_operator__1
  :: DM -> Vector String -> IO String
casadi_print_operator__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_print_operator__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_print_operator__2" c_casadi_print_operator__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr StdString)

casadi_print_operator__2
  :: IM -> Vector String -> IO String
casadi_print_operator__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_print_operator__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_print_operator__3" c_casadi_print_operator__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr StdString)

casadi_print_operator__3
  :: MX -> Vector String -> IO String
casadi_print_operator__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_print_operator__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_project__0" c_casadi_project__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr Sparsity' -> IO (Ptr SX')

casadi_project__0
  :: SX -> Sparsity -> IO SX
casadi_project__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_project__1" c_casadi_project__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr Sparsity' -> CInt -> IO (Ptr SX')

casadi_project__1
  :: SX -> Sparsity -> Bool -> IO SX
casadi_project__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_project__2" c_casadi_project__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr Sparsity' -> IO (Ptr DM')

casadi_project__2
  :: DM -> Sparsity -> IO DM
casadi_project__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_project__3" c_casadi_project__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr Sparsity' -> CInt -> IO (Ptr DM')

casadi_project__3
  :: DM -> Sparsity -> Bool -> IO DM
casadi_project__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_project__4" c_casadi_project__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr Sparsity' -> IO (Ptr IM')

casadi_project__4
  :: IM -> Sparsity -> IO IM
casadi_project__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_project__5" c_casadi_project__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr Sparsity' -> CInt -> IO (Ptr IM')

casadi_project__5
  :: IM -> Sparsity -> Bool -> IO IM
casadi_project__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_project__6" c_casadi_project__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr Sparsity' -> IO (Ptr MX')

casadi_project__6
  :: MX -> Sparsity -> IO MX
casadi_project__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_project__7" c_casadi_project__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr Sparsity' -> CInt -> IO (Ptr MX')

casadi_project__7
  :: MX -> Sparsity -> Bool -> IO MX
casadi_project__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_project__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pw_const__0" c_casadi_pw_const__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_pw_const__0
  :: SX -> SX -> SX -> IO SX
casadi_pw_const__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pw_const__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pw_const__1" c_casadi_pw_const__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_pw_const__1
  :: DM -> DM -> DM -> IO DM
casadi_pw_const__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pw_const__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pw_const__2" c_casadi_pw_const__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_pw_const__2
  :: IM -> IM -> IM -> IO IM
casadi_pw_const__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pw_const__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pw_lin__0" c_casadi_pw_lin__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_pw_lin__0
  :: SX -> SX -> SX -> IO SX
casadi_pw_lin__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pw_lin__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pw_lin__1" c_casadi_pw_lin__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_pw_lin__1
  :: DM -> DM -> DM -> IO DM
casadi_pw_lin__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pw_lin__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_pw_lin__2" c_casadi_pw_lin__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_pw_lin__2
  :: IM -> IM -> IM -> IO IM
casadi_pw_lin__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_pw_lin__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_qr__0" c_casadi_qr__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO ()

casadi_qr__0
  :: SX -> SX -> SX -> IO ()
casadi_qr__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_qr__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_qr__1" c_casadi_qr__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO ()

casadi_qr__1
  :: DM -> DM -> DM -> IO ()
casadi_qr__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_qr__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_qr__2" c_casadi_qr__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO ()

casadi_qr__2
  :: IM -> IM -> IM -> IO ()
casadi_qr__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_qr__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_ramp__0" c_casadi_ramp__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_ramp__0
  :: SX -> IO SX
casadi_ramp__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ramp__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_ramp__1" c_casadi_ramp__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_ramp__1
  :: DM -> IO DM
casadi_ramp__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ramp__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_ramp__2" c_casadi_ramp__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_ramp__2
  :: IM -> IO IM
casadi_ramp__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_ramp__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_rank1__0" c_casadi_rank1__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_rank1__0
  :: SX -> SX -> SX -> SX -> IO SX
casadi_rank1__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rank1__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_rank1__1" c_casadi_rank1__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_rank1__1
  :: DM -> DM -> DM -> DM -> IO DM
casadi_rank1__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rank1__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_rank1__2" c_casadi_rank1__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_rank1__2
  :: IM -> IM -> IM -> IM -> IO IM
casadi_rank1__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rank1__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_rank1__3" c_casadi_rank1__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_rank1__3
  :: MX -> MX -> MX -> MX -> IO MX
casadi_rank1__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rank1__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_rdivide__0" c_casadi_rdivide__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_rdivide__0
  :: Double -> Double -> IO Double
casadi_rdivide__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rdivide__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_rdivide__1" c_casadi_rdivide__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_rdivide__1
  :: SX -> SX -> IO SX
casadi_rdivide__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rdivide__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_rdivide__2" c_casadi_rdivide__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_rdivide__2
  :: DM -> DM -> IO DM
casadi_rdivide__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rdivide__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_rdivide__3" c_casadi_rdivide__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_rdivide__3
  :: IM -> IM -> IO IM
casadi_rdivide__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rdivide__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_rdivide__4" c_casadi_rdivide__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_rdivide__4
  :: MX -> MX -> IO MX
casadi_rdivide__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rdivide__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_rectangle__0" c_casadi_rectangle__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_rectangle__0
  :: SX -> IO SX
casadi_rectangle__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rectangle__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_rectangle__1" c_casadi_rectangle__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_rectangle__1
  :: DM -> IO DM
casadi_rectangle__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rectangle__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_rectangle__2" c_casadi_rectangle__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_rectangle__2
  :: IM -> IO IM
casadi_rectangle__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_rectangle__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_repmat__0" c_casadi_repmat__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdPair CInt CInt) -> IO (Ptr SX')

casadi_repmat__0
  :: SX -> (Int, Int) -> IO SX
casadi_repmat__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__1" c_casadi_repmat__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_repmat__1
  :: SX -> Int -> IO SX
casadi_repmat__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__2" c_casadi_repmat__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> CInt -> IO (Ptr SX')

casadi_repmat__2
  :: SX -> Int -> Int -> IO SX
casadi_repmat__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repmat__3" c_casadi_repmat__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdPair CInt CInt) -> IO (Ptr DM')

casadi_repmat__3
  :: DM -> (Int, Int) -> IO DM
casadi_repmat__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__4" c_casadi_repmat__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_repmat__4
  :: DM -> Int -> IO DM
casadi_repmat__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__5" c_casadi_repmat__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> CInt -> IO (Ptr DM')

casadi_repmat__5
  :: DM -> Int -> Int -> IO DM
casadi_repmat__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repmat__6" c_casadi_repmat__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi_repmat__6
  :: IM -> (Int, Int) -> IO IM
casadi_repmat__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__7" c_casadi_repmat__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_repmat__7
  :: IM -> Int -> IO IM
casadi_repmat__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__8" c_casadi_repmat__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO (Ptr IM')

casadi_repmat__8
  :: IM -> Int -> Int -> IO IM
casadi_repmat__8 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__8 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repmat__9" c_casadi_repmat__9
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdPair CInt CInt) -> IO (Ptr MX')

casadi_repmat__9
  :: MX -> (Int, Int) -> IO MX
casadi_repmat__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__10" c_casadi_repmat__10
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_repmat__10
  :: MX -> Int -> IO MX
casadi_repmat__10 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__10 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__11" c_casadi_repmat__11
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> CInt -> IO (Ptr MX')

casadi_repmat__11
  :: MX -> Int -> Int -> IO MX
casadi_repmat__11 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__11 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repmat__12" c_casadi_repmat__12
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdPair CInt CInt) -> IO (Ptr Sparsity')

casadi_repmat__12
  :: Sparsity -> (Int, Int) -> IO Sparsity
casadi_repmat__12 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__12 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__13" c_casadi_repmat__13
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')

casadi_repmat__13
  :: Sparsity -> Int -> IO Sparsity
casadi_repmat__13 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__13 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repmat__14" c_casadi_repmat__14
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr Sparsity')

casadi_repmat__14
  :: Sparsity -> Int -> Int -> IO Sparsity
casadi_repmat__14 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repmat__14 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repsum__0" c_casadi_repsum__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_repsum__0
  :: SX -> Int -> IO SX
casadi_repsum__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repsum__1" c_casadi_repsum__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> CInt -> IO (Ptr SX')

casadi_repsum__1
  :: SX -> Int -> Int -> IO SX
casadi_repsum__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repsum__2" c_casadi_repsum__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_repsum__2
  :: DM -> Int -> IO DM
casadi_repsum__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repsum__3" c_casadi_repsum__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> CInt -> IO (Ptr DM')

casadi_repsum__3
  :: DM -> Int -> Int -> IO DM
casadi_repsum__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repsum__4" c_casadi_repsum__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_repsum__4
  :: IM -> Int -> IO IM
casadi_repsum__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repsum__5" c_casadi_repsum__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO (Ptr IM')

casadi_repsum__5
  :: IM -> Int -> Int -> IO IM
casadi_repsum__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_repsum__6" c_casadi_repsum__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_repsum__6
  :: MX -> Int -> IO MX
casadi_repsum__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_repsum__7" c_casadi_repsum__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> CInt -> IO (Ptr MX')

casadi_repsum__7
  :: MX -> Int -> Int -> IO MX
casadi_repsum__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_repsum__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reshape__0" c_casadi_reshape__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr Sparsity' -> IO (Ptr SX')

casadi_reshape__0
  :: SX -> Sparsity -> IO SX
casadi_reshape__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__1" c_casadi_reshape__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdPair CInt CInt) -> IO (Ptr SX')

casadi_reshape__1
  :: SX -> (Int, Int) -> IO SX
casadi_reshape__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__2" c_casadi_reshape__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> CInt -> IO (Ptr SX')

casadi_reshape__2
  :: SX -> Int -> Int -> IO SX
casadi_reshape__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reshape__3" c_casadi_reshape__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr Sparsity' -> IO (Ptr DM')

casadi_reshape__3
  :: DM -> Sparsity -> IO DM
casadi_reshape__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__4" c_casadi_reshape__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdPair CInt CInt) -> IO (Ptr DM')

casadi_reshape__4
  :: DM -> (Int, Int) -> IO DM
casadi_reshape__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__5" c_casadi_reshape__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> CInt -> IO (Ptr DM')

casadi_reshape__5
  :: DM -> Int -> Int -> IO DM
casadi_reshape__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reshape__6" c_casadi_reshape__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr Sparsity' -> IO (Ptr IM')

casadi_reshape__6
  :: IM -> Sparsity -> IO IM
casadi_reshape__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__7" c_casadi_reshape__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi_reshape__7
  :: IM -> (Int, Int) -> IO IM
casadi_reshape__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__8" c_casadi_reshape__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO (Ptr IM')

casadi_reshape__8
  :: IM -> Int -> Int -> IO IM
casadi_reshape__8 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__8 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reshape__9" c_casadi_reshape__9
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr Sparsity' -> IO (Ptr MX')

casadi_reshape__9
  :: MX -> Sparsity -> IO MX
casadi_reshape__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__10" c_casadi_reshape__10
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdPair CInt CInt) -> IO (Ptr MX')

casadi_reshape__10
  :: MX -> (Int, Int) -> IO MX
casadi_reshape__10 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__10 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__11" c_casadi_reshape__11
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> CInt -> IO (Ptr MX')

casadi_reshape__11
  :: MX -> Int -> Int -> IO MX
casadi_reshape__11 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__11 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reshape__12" c_casadi_reshape__12
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_reshape__12
  :: Sparsity -> Sparsity -> IO Sparsity
casadi_reshape__12 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__12 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__13" c_casadi_reshape__13
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdPair CInt CInt) -> IO (Ptr Sparsity')

casadi_reshape__13
  :: Sparsity -> (Int, Int) -> IO Sparsity
casadi_reshape__13 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__13 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_reshape__14" c_casadi_reshape__14
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr Sparsity')

casadi_reshape__14
  :: Sparsity -> Int -> Int -> IO Sparsity
casadi_reshape__14 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reshape__14 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reverse__0" c_casadi_reverse__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr (StdVec (Ptr SX')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_reverse__0
  :: Vector SX -> Vector SX -> Vector (Vector SX) -> IO (Vector (Vector SX))
casadi_reverse__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reverse__1" c_casadi_reverse__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr (StdVec (Ptr SX')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi_reverse__1
  :: Vector SX -> Vector SX -> Vector (Vector SX) -> M.Map String GenericType -> IO (Vector (Vector SX))
casadi_reverse__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_reverse__2" c_casadi_reverse__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr (StdVec (Ptr DM')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_reverse__2
  :: Vector DM -> Vector DM -> Vector (Vector DM) -> IO (Vector (Vector DM))
casadi_reverse__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reverse__3" c_casadi_reverse__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr (StdVec (Ptr DM')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi_reverse__3
  :: Vector DM -> Vector DM -> Vector (Vector DM) -> M.Map String GenericType -> IO (Vector (Vector DM))
casadi_reverse__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_reverse__4" c_casadi_reverse__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr (StdVec (Ptr IM')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_reverse__4
  :: Vector IM -> Vector IM -> Vector (Vector IM) -> IO (Vector (Vector IM))
casadi_reverse__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reverse__5" c_casadi_reverse__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr (StdVec (Ptr IM')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi_reverse__5
  :: Vector IM -> Vector IM -> Vector (Vector IM) -> M.Map String GenericType -> IO (Vector (Vector IM))
casadi_reverse__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_reverse__6" c_casadi_reverse__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr (StdVec (Ptr MX')))) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_reverse__6
  :: Vector MX -> Vector MX -> Vector (Vector MX) -> IO (Vector (Vector MX))
casadi_reverse__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_reverse__7" c_casadi_reverse__7
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr (StdVec (Ptr MX')))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi_reverse__7
  :: Vector MX -> Vector MX -> Vector (Vector MX) -> M.Map String GenericType -> IO (Vector (Vector MX))
casadi_reverse__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_reverse__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_shared__0" c_casadi_shared__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> IO ()

casadi_shared__0
  :: Vector SX -> Vector SX -> Vector SX -> Vector SX -> IO ()
casadi_shared__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_shared__1" c_casadi_shared__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr StdString -> IO ()

casadi_shared__1
  :: Vector SX -> Vector SX -> Vector SX -> Vector SX -> String -> IO ()
casadi_shared__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__1 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



foreign import ccall unsafe "casadi_shared__2" c_casadi_shared__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr StdString -> Ptr StdString -> IO ()

casadi_shared__2
  :: Vector SX -> Vector SX -> Vector SX -> Vector SX -> String -> String -> IO ()
casadi_shared__2 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__2 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ()



foreign import ccall unsafe "casadi_shared__3" c_casadi_shared__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> IO ()

casadi_shared__3
  :: Vector DM -> Vector DM -> Vector DM -> Vector DM -> IO ()
casadi_shared__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_shared__4" c_casadi_shared__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr StdString -> IO ()

casadi_shared__4
  :: Vector DM -> Vector DM -> Vector DM -> Vector DM -> String -> IO ()
casadi_shared__4 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__4 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



foreign import ccall unsafe "casadi_shared__5" c_casadi_shared__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr StdString -> Ptr StdString -> IO ()

casadi_shared__5
  :: Vector DM -> Vector DM -> Vector DM -> Vector DM -> String -> String -> IO ()
casadi_shared__5 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__5 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ()



foreign import ccall unsafe "casadi_shared__6" c_casadi_shared__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> IO ()

casadi_shared__6
  :: Vector IM -> Vector IM -> Vector IM -> Vector IM -> IO ()
casadi_shared__6 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__6 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_shared__7" c_casadi_shared__7
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr StdString -> IO ()

casadi_shared__7
  :: Vector IM -> Vector IM -> Vector IM -> Vector IM -> String -> IO ()
casadi_shared__7 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__7 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



foreign import ccall unsafe "casadi_shared__8" c_casadi_shared__8
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr StdString -> Ptr StdString -> IO ()

casadi_shared__8
  :: Vector IM -> Vector IM -> Vector IM -> Vector IM -> String -> String -> IO ()
casadi_shared__8 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__8 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ()



foreign import ccall unsafe "casadi_shared__9" c_casadi_shared__9
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> IO ()

casadi_shared__9
  :: Vector MX -> Vector MX -> Vector MX -> Vector MX -> IO ()
casadi_shared__9 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__9 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_shared__10" c_casadi_shared__10
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr StdString -> IO ()

casadi_shared__10
  :: Vector MX -> Vector MX -> Vector MX -> Vector MX -> String -> IO ()
casadi_shared__10 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__10 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



foreign import ccall unsafe "casadi_shared__11" c_casadi_shared__11
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr StdString -> Ptr StdString -> IO ()

casadi_shared__11
  :: Vector MX -> Vector MX -> Vector MX -> Vector MX -> String -> String -> IO ()
casadi_shared__11 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_shared__11 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ()



foreign import ccall unsafe "casadi_sign__0" c_casadi_sign__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_sign__0
  :: Double -> IO Double
casadi_sign__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sign__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sign__1" c_casadi_sign__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sign__1
  :: SX -> IO SX
casadi_sign__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sign__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sign__2" c_casadi_sign__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sign__2
  :: DM -> IO DM
casadi_sign__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sign__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sign__3" c_casadi_sign__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sign__3
  :: IM -> IO IM
casadi_sign__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sign__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sign__4" c_casadi_sign__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_sign__4
  :: MX -> IO MX
casadi_sign__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sign__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_simplify__0" c_casadi_simplify__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_simplify__0
  :: Double -> IO Double
casadi_simplify__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_simplify__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_simplify__1" c_casadi_simplify__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_simplify__1
  :: SX -> IO SX
casadi_simplify__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_simplify__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_simplify__2" c_casadi_simplify__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_simplify__2
  :: DM -> IO DM
casadi_simplify__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_simplify__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_simplify__3" c_casadi_simplify__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_simplify__3
  :: IM -> IO IM
casadi_simplify__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_simplify__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_simplify__4" c_casadi_simplify__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_simplify__4
  :: MX -> IO MX
casadi_simplify__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_simplify__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sin__0" c_casadi_sin__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_sin__0
  :: Double -> IO Double
casadi_sin__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sin__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sin__1" c_casadi_sin__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sin__1
  :: SX -> IO SX
casadi_sin__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sin__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sin__2" c_casadi_sin__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sin__2
  :: DM -> IO DM
casadi_sin__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sin__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sin__3" c_casadi_sin__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sin__3
  :: IM -> IO IM
casadi_sin__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sin__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sin__4" c_casadi_sin__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_sin__4
  :: MX -> IO MX
casadi_sin__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sin__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sinh__0" c_casadi_sinh__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_sinh__0
  :: Double -> IO Double
casadi_sinh__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sinh__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sinh__1" c_casadi_sinh__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sinh__1
  :: SX -> IO SX
casadi_sinh__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sinh__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sinh__2" c_casadi_sinh__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sinh__2
  :: DM -> IO DM
casadi_sinh__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sinh__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sinh__3" c_casadi_sinh__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sinh__3
  :: IM -> IO IM
casadi_sinh__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sinh__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sinh__4" c_casadi_sinh__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_sinh__4
  :: MX -> IO MX
casadi_sinh__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sinh__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_skew__0" c_casadi_skew__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_skew__0
  :: SX -> IO SX
casadi_skew__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_skew__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_skew__1" c_casadi_skew__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_skew__1
  :: DM -> IO DM
casadi_skew__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_skew__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_skew__2" c_casadi_skew__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_skew__2
  :: IM -> IO IM
casadi_skew__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_skew__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_skew__3" c_casadi_skew__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_skew__3
  :: MX -> IO MX
casadi_skew__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_skew__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_solve__0" c_casadi_solve__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr StdString -> IO (Ptr SX')

casadi_solve__0
  :: SX -> SX -> String -> IO SX
casadi_solve__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_solve__1" c_casadi_solve__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr SX')

casadi_solve__1
  :: SX -> SX -> String -> M.Map String GenericType -> IO SX
casadi_solve__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_solve__2" c_casadi_solve__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_solve__2
  :: SX -> SX -> IO SX
casadi_solve__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_solve__3" c_casadi_solve__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr StdString -> IO (Ptr DM')

casadi_solve__3
  :: DM -> DM -> String -> IO DM
casadi_solve__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_solve__4" c_casadi_solve__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr DM')

casadi_solve__4
  :: DM -> DM -> String -> M.Map String GenericType -> IO DM
casadi_solve__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_solve__5" c_casadi_solve__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_solve__5
  :: DM -> DM -> IO DM
casadi_solve__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_solve__6" c_casadi_solve__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr StdString -> IO (Ptr IM')

casadi_solve__6
  :: IM -> IM -> String -> IO IM
casadi_solve__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_solve__7" c_casadi_solve__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr IM')

casadi_solve__7
  :: IM -> IM -> String -> M.Map String GenericType -> IO IM
casadi_solve__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_solve__8" c_casadi_solve__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_solve__8
  :: IM -> IM -> IO IM
casadi_solve__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_solve__9" c_casadi_solve__9
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr StdString -> IO (Ptr MX')

casadi_solve__9
  :: MX -> MX -> String -> IO MX
casadi_solve__9 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__9 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_solve__10" c_casadi_solve__10
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr MX')

casadi_solve__10
  :: MX -> MX -> String -> M.Map String GenericType -> IO MX
casadi_solve__10 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__10 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_solve__11" c_casadi_solve__11
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_solve__11
  :: MX -> MX -> IO MX
casadi_solve__11 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_solve__11 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_sparsify__0" c_casadi_sparsify__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sparsify__0
  :: SX -> IO SX
casadi_sparsify__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sparsify__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sparsify__1" c_casadi_sparsify__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CDouble -> IO (Ptr SX')

casadi_sparsify__1
  :: SX -> Double -> IO SX
casadi_sparsify__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sparsify__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_sparsify__2" c_casadi_sparsify__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sparsify__2
  :: DM -> IO DM
casadi_sparsify__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sparsify__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sparsify__3" c_casadi_sparsify__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> CDouble -> IO (Ptr DM')

casadi_sparsify__3
  :: DM -> Double -> IO DM
casadi_sparsify__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sparsify__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_sparsify__4" c_casadi_sparsify__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sparsify__4
  :: IM -> IO IM
casadi_sparsify__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sparsify__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sparsify__5" c_casadi_sparsify__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> CDouble -> IO (Ptr IM')

casadi_sparsify__5
  :: IM -> Double -> IO IM
casadi_sparsify__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sparsify__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_sprank__0" c_casadi_sprank__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi_sprank__0
  :: SX -> IO Int
casadi_sprank__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sprank__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sprank__1" c_casadi_sprank__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi_sprank__1
  :: DM -> IO Int
casadi_sprank__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sprank__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sprank__2" c_casadi_sprank__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi_sprank__2
  :: IM -> IO Int
casadi_sprank__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sprank__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sprank__3" c_casadi_sprank__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi_sprank__3
  :: MX -> IO Int
casadi_sprank__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sprank__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sprank__4" c_casadi_sprank__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO CInt

casadi_sprank__4
  :: Sparsity -> IO Int
casadi_sprank__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sprank__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sqrt__0" c_casadi_sqrt__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_sqrt__0
  :: Double -> IO Double
casadi_sqrt__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sqrt__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sqrt__1" c_casadi_sqrt__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sqrt__1
  :: SX -> IO SX
casadi_sqrt__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sqrt__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sqrt__2" c_casadi_sqrt__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sqrt__2
  :: DM -> IO DM
casadi_sqrt__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sqrt__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sqrt__3" c_casadi_sqrt__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sqrt__3
  :: IM -> IO IM
casadi_sqrt__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sqrt__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sqrt__4" c_casadi_sqrt__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_sqrt__4
  :: MX -> IO MX
casadi_sqrt__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sqrt__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_substitute__0" c_casadi_substitute__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr (StdVec (Ptr SX')))

casadi_substitute__0
  :: Vector SX -> Vector SX -> Vector SX -> IO (Vector SX)
casadi_substitute__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute__1" c_casadi_substitute__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_substitute__1
  :: SX -> SX -> SX -> IO SX
casadi_substitute__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute__2" c_casadi_substitute__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> IO (Ptr (StdVec (Ptr DM')))

casadi_substitute__2
  :: Vector DM -> Vector DM -> Vector DM -> IO (Vector DM)
casadi_substitute__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute__3" c_casadi_substitute__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_substitute__3
  :: DM -> DM -> DM -> IO DM
casadi_substitute__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute__4" c_casadi_substitute__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> IO (Ptr (StdVec (Ptr IM')))

casadi_substitute__4
  :: Vector IM -> Vector IM -> Vector IM -> IO (Vector IM)
casadi_substitute__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute__5" c_casadi_substitute__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_substitute__5
  :: IM -> IM -> IM -> IO IM
casadi_substitute__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute__6" c_casadi_substitute__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr (StdVec (Ptr MX')))

casadi_substitute__6
  :: Vector MX -> Vector MX -> Vector MX -> IO (Vector MX)
casadi_substitute__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute__7" c_casadi_substitute__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_substitute__7
  :: MX -> MX -> MX -> IO MX
casadi_substitute__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_substitute_inplace__0" c_casadi_substitute_inplace__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> IO ()

casadi_substitute_inplace__0
  :: Vector SX -> Vector SX -> Vector SX -> IO ()
casadi_substitute_inplace__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_substitute_inplace__1" c_casadi_substitute_inplace__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> CInt -> IO ()

casadi_substitute_inplace__1
  :: Vector SX -> Vector SX -> Vector SX -> Bool -> IO ()
casadi_substitute_inplace__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_substitute_inplace__2" c_casadi_substitute_inplace__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> IO ()

casadi_substitute_inplace__2
  :: Vector DM -> Vector DM -> Vector DM -> IO ()
casadi_substitute_inplace__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_substitute_inplace__3" c_casadi_substitute_inplace__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> CInt -> IO ()

casadi_substitute_inplace__3
  :: Vector DM -> Vector DM -> Vector DM -> Bool -> IO ()
casadi_substitute_inplace__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_substitute_inplace__4" c_casadi_substitute_inplace__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> IO ()

casadi_substitute_inplace__4
  :: Vector IM -> Vector IM -> Vector IM -> IO ()
casadi_substitute_inplace__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_substitute_inplace__5" c_casadi_substitute_inplace__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> Ptr (StdVec (Ptr IM')) -> CInt -> IO ()

casadi_substitute_inplace__5
  :: Vector IM -> Vector IM -> Vector IM -> Bool -> IO ()
casadi_substitute_inplace__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_substitute_inplace__6" c_casadi_substitute_inplace__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> IO ()

casadi_substitute_inplace__6
  :: Vector MX -> Vector MX -> Vector MX -> IO ()
casadi_substitute_inplace__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



foreign import ccall unsafe "casadi_substitute_inplace__7" c_casadi_substitute_inplace__7
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> CInt -> IO ()

casadi_substitute_inplace__7
  :: Vector MX -> Vector MX -> Vector MX -> Bool -> IO ()
casadi_substitute_inplace__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_substitute_inplace__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



foreign import ccall unsafe "casadi_sum1__0" c_casadi_sum1__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sum1__0
  :: SX -> IO SX
casadi_sum1__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum1__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum1__1" c_casadi_sum1__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sum1__1
  :: DM -> IO DM
casadi_sum1__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum1__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum1__2" c_casadi_sum1__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sum1__2
  :: IM -> IO IM
casadi_sum1__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum1__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum1__3" c_casadi_sum1__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_sum1__3
  :: MX -> IO MX
casadi_sum1__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum1__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum2__0" c_casadi_sum2__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sum2__0
  :: SX -> IO SX
casadi_sum2__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum2__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum2__1" c_casadi_sum2__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sum2__1
  :: DM -> IO DM
casadi_sum2__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum2__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum2__2" c_casadi_sum2__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sum2__2
  :: IM -> IO IM
casadi_sum2__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum2__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum2__3" c_casadi_sum2__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_sum2__3
  :: MX -> IO MX
casadi_sum2__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum2__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum_square__0" c_casadi_sum_square__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_sum_square__0
  :: SX -> IO SX
casadi_sum_square__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum_square__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum_square__1" c_casadi_sum_square__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_sum_square__1
  :: DM -> IO DM
casadi_sum_square__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum_square__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum_square__2" c_casadi_sum_square__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_sum_square__2
  :: IM -> IO IM
casadi_sum_square__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum_square__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_sum_square__3" c_casadi_sum_square__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_sum_square__3
  :: MX -> IO MX
casadi_sum_square__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_sum_square__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_symvar__0" c_casadi_symvar__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec (Ptr SX')))

casadi_symvar__0
  :: SX -> IO (Vector SX)
casadi_symvar__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_symvar__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_symvar__1" c_casadi_symvar__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec (Ptr DM')))

casadi_symvar__1
  :: DM -> IO (Vector DM)
casadi_symvar__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_symvar__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_symvar__2" c_casadi_symvar__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec (Ptr IM')))

casadi_symvar__2
  :: IM -> IO (Vector IM)
casadi_symvar__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_symvar__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_symvar__3" c_casadi_symvar__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec (Ptr MX')))

casadi_symvar__3
  :: MX -> IO (Vector MX)
casadi_symvar__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_symvar__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tan__0" c_casadi_tan__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_tan__0
  :: Double -> IO Double
casadi_tan__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tan__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tan__1" c_casadi_tan__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_tan__1
  :: SX -> IO SX
casadi_tan__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tan__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tan__2" c_casadi_tan__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_tan__2
  :: DM -> IO DM
casadi_tan__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tan__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tan__3" c_casadi_tan__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_tan__3
  :: IM -> IO IM
casadi_tan__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tan__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tan__4" c_casadi_tan__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_tan__4
  :: MX -> IO MX
casadi_tan__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tan__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tangent__0" c_casadi_tangent__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_tangent__0
  :: SX -> SX -> IO SX
casadi_tangent__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tangent__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tangent__1" c_casadi_tangent__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_tangent__1
  :: DM -> DM -> IO DM
casadi_tangent__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tangent__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tangent__2" c_casadi_tangent__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_tangent__2
  :: IM -> IM -> IO IM
casadi_tangent__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tangent__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tangent__3" c_casadi_tangent__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_tangent__3
  :: MX -> MX -> IO MX
casadi_tangent__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tangent__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tanh__0" c_casadi_tanh__0
  :: Ptr (Ptr StdString) -> CDouble -> IO CDouble

casadi_tanh__0
  :: Double -> IO Double
casadi_tanh__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tanh__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tanh__1" c_casadi_tanh__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_tanh__1
  :: SX -> IO SX
casadi_tanh__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tanh__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tanh__2" c_casadi_tanh__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_tanh__2
  :: DM -> IO DM
casadi_tanh__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tanh__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tanh__3" c_casadi_tanh__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_tanh__3
  :: IM -> IO IM
casadi_tanh__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tanh__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tanh__4" c_casadi_tanh__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_tanh__4
  :: MX -> IO MX
casadi_tanh__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tanh__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_taylor__0" c_casadi_taylor__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_taylor__0
  :: SX -> SX -> IO SX
casadi_taylor__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_taylor__1" c_casadi_taylor__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_taylor__1
  :: SX -> SX -> SX -> IO SX
casadi_taylor__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_taylor__2" c_casadi_taylor__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_taylor__2
  :: SX -> SX -> SX -> Int -> IO SX
casadi_taylor__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_taylor__3" c_casadi_taylor__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_taylor__3
  :: DM -> DM -> IO DM
casadi_taylor__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_taylor__4" c_casadi_taylor__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_taylor__4
  :: DM -> DM -> DM -> IO DM
casadi_taylor__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_taylor__5" c_casadi_taylor__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_taylor__5
  :: DM -> DM -> DM -> Int -> IO DM
casadi_taylor__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_taylor__6" c_casadi_taylor__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_taylor__6
  :: IM -> IM -> IO IM
casadi_taylor__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_taylor__7" c_casadi_taylor__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_taylor__7
  :: IM -> IM -> IM -> IO IM
casadi_taylor__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "casadi_taylor__8" c_casadi_taylor__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_taylor__8
  :: IM -> IM -> IM -> Int -> IO IM
casadi_taylor__8 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_taylor__8 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_times__0" c_casadi_times__0
  :: Ptr (Ptr StdString) -> CDouble -> CDouble -> IO CDouble

casadi_times__0
  :: Double -> Double -> IO Double
casadi_times__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_times__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_times__1" c_casadi_times__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_times__1
  :: SX -> SX -> IO SX
casadi_times__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_times__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_times__2" c_casadi_times__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_times__2
  :: DM -> DM -> IO DM
casadi_times__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_times__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_times__3" c_casadi_times__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_times__3
  :: IM -> IM -> IO IM
casadi_times__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_times__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_times__4" c_casadi_times__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_times__4
  :: MX -> MX -> IO MX
casadi_times__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_times__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_trace__0" c_casadi_trace__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_trace__0
  :: SX -> IO SX
casadi_trace__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_trace__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_trace__1" c_casadi_trace__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_trace__1
  :: DM -> IO DM
casadi_trace__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_trace__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_trace__2" c_casadi_trace__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_trace__2
  :: IM -> IO IM
casadi_trace__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_trace__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_trace__3" c_casadi_trace__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_trace__3
  :: MX -> IO MX
casadi_trace__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_trace__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_transpose__0" c_casadi_transpose__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_transpose__0
  :: SX -> IO SX
casadi_transpose__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_transpose__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_transpose__1" c_casadi_transpose__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_transpose__1
  :: DM -> IO DM
casadi_transpose__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_transpose__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_transpose__2" c_casadi_transpose__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_transpose__2
  :: IM -> IO IM
casadi_transpose__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_transpose__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_transpose__3" c_casadi_transpose__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_transpose__3
  :: MX -> IO MX
casadi_transpose__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_transpose__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_transpose__4" c_casadi_transpose__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_transpose__4
  :: Sparsity -> IO Sparsity
casadi_transpose__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_transpose__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triangle__0" c_casadi_triangle__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_triangle__0
  :: SX -> IO SX
casadi_triangle__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triangle__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triangle__1" c_casadi_triangle__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_triangle__1
  :: DM -> IO DM
casadi_triangle__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triangle__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triangle__2" c_casadi_triangle__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_triangle__2
  :: IM -> IO IM
casadi_triangle__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triangle__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril__0" c_casadi_tril__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_tril__0
  :: SX -> IO SX
casadi_tril__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril__1" c_casadi_tril__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_tril__1
  :: SX -> Bool -> IO SX
casadi_tril__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tril__2" c_casadi_tril__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_tril__2
  :: DM -> IO DM
casadi_tril__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril__3" c_casadi_tril__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_tril__3
  :: DM -> Bool -> IO DM
casadi_tril__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tril__4" c_casadi_tril__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_tril__4
  :: IM -> IO IM
casadi_tril__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril__5" c_casadi_tril__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_tril__5
  :: IM -> Bool -> IO IM
casadi_tril__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tril__6" c_casadi_tril__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_tril__6
  :: MX -> IO MX
casadi_tril__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril__7" c_casadi_tril__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_tril__7
  :: MX -> Bool -> IO MX
casadi_tril__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tril__8" c_casadi_tril__8
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_tril__8
  :: Sparsity -> IO Sparsity
casadi_tril__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril__9" c_casadi_tril__9
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')

casadi_tril__9
  :: Sparsity -> Bool -> IO Sparsity
casadi_tril__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_tril2symm__0" c_casadi_tril2symm__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_tril2symm__0
  :: SX -> IO SX
casadi_tril2symm__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril2symm__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril2symm__1" c_casadi_tril2symm__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_tril2symm__1
  :: DM -> IO DM
casadi_tril2symm__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril2symm__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril2symm__2" c_casadi_tril2symm__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_tril2symm__2
  :: IM -> IO IM
casadi_tril2symm__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril2symm__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_tril2symm__3" c_casadi_tril2symm__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_tril2symm__3
  :: MX -> IO MX
casadi_tril2symm__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_tril2symm__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu__0" c_casadi_triu__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_triu__0
  :: SX -> IO SX
casadi_triu__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu__1" c_casadi_triu__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr SX')

casadi_triu__1
  :: SX -> Bool -> IO SX
casadi_triu__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_triu__2" c_casadi_triu__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_triu__2
  :: DM -> IO DM
casadi_triu__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu__3" c_casadi_triu__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr DM')

casadi_triu__3
  :: DM -> Bool -> IO DM
casadi_triu__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_triu__4" c_casadi_triu__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_triu__4
  :: IM -> IO IM
casadi_triu__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu__5" c_casadi_triu__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi_triu__5
  :: IM -> Bool -> IO IM
casadi_triu__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_triu__6" c_casadi_triu__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_triu__6
  :: MX -> IO MX
casadi_triu__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu__7" c_casadi_triu__7
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi_triu__7
  :: MX -> Bool -> IO MX
casadi_triu__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_triu__8" c_casadi_triu__8
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_triu__8
  :: Sparsity -> IO Sparsity
casadi_triu__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu__9" c_casadi_triu__9
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr Sparsity')

casadi_triu__9
  :: Sparsity -> Bool -> IO Sparsity
casadi_triu__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_triu2symm__0" c_casadi_triu2symm__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_triu2symm__0
  :: SX -> IO SX
casadi_triu2symm__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu2symm__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu2symm__1" c_casadi_triu2symm__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_triu2symm__1
  :: DM -> IO DM
casadi_triu2symm__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu2symm__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu2symm__2" c_casadi_triu2symm__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_triu2symm__2
  :: IM -> IO IM
casadi_triu2symm__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu2symm__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_triu2symm__3" c_casadi_triu2symm__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_triu2symm__3
  :: MX -> IO MX
casadi_triu2symm__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_triu2symm__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_unite__0" c_casadi_unite__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi_unite__0
  :: SX -> SX -> IO SX
casadi_unite__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_unite__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_unite__1" c_casadi_unite__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi_unite__1
  :: DM -> DM -> IO DM
casadi_unite__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_unite__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_unite__2" c_casadi_unite__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi_unite__2
  :: IM -> IM -> IO IM
casadi_unite__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_unite__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_unite__3" c_casadi_unite__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi_unite__3
  :: MX -> MX -> IO MX
casadi_unite__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_unite__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vec__0" c_casadi_vec__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi_vec__0
  :: SX -> IO SX
casadi_vec__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vec__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vec__1" c_casadi_vec__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi_vec__1
  :: DM -> IO DM
casadi_vec__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vec__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vec__2" c_casadi_vec__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi_vec__2
  :: IM -> IO IM
casadi_vec__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vec__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vec__3" c_casadi_vec__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi_vec__3
  :: MX -> IO MX
casadi_vec__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vec__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vec__4" c_casadi_vec__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr Sparsity')

casadi_vec__4
  :: Sparsity -> IO Sparsity
casadi_vec__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vec__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_veccat__0" c_casadi_veccat__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr SX')

casadi_veccat__0
  :: Vector SX -> IO SX
casadi_veccat__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_veccat__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_veccat__1" c_casadi_veccat__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> IO (Ptr DM')

casadi_veccat__1
  :: Vector DM -> IO DM
casadi_veccat__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_veccat__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_veccat__2" c_casadi_veccat__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> IO (Ptr IM')

casadi_veccat__2
  :: Vector IM -> IO IM
casadi_veccat__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_veccat__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_veccat__3" c_casadi_veccat__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi_veccat__3
  :: Vector MX -> IO MX
casadi_veccat__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_veccat__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_veccat__4" c_casadi_veccat__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Sparsity')) -> IO (Ptr Sparsity')

casadi_veccat__4
  :: Vector Sparsity -> IO Sparsity
casadi_veccat__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_veccat__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertcat__0" c_casadi_vertcat__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr SX')

casadi_vertcat__0
  :: Vector SX -> IO SX
casadi_vertcat__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertcat__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertcat__1" c_casadi_vertcat__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> IO (Ptr DM')

casadi_vertcat__1
  :: Vector DM -> IO DM
casadi_vertcat__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertcat__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertcat__2" c_casadi_vertcat__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr IM')) -> IO (Ptr IM')

casadi_vertcat__2
  :: Vector IM -> IO IM
casadi_vertcat__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertcat__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertcat__3" c_casadi_vertcat__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi_vertcat__3
  :: Vector MX -> IO MX
casadi_vertcat__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertcat__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertcat__4" c_casadi_vertcat__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Sparsity')) -> IO (Ptr Sparsity')

casadi_vertcat__4
  :: Vector Sparsity -> IO Sparsity
casadi_vertcat__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertcat__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertsplit__0" c_casadi_vertsplit__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec (Ptr SX')))

casadi_vertsplit__0
  :: SX -> IO (Vector SX)
casadi_vertsplit__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertsplit__1" c_casadi_vertsplit__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr (StdVec (Ptr SX')))

casadi_vertsplit__1
  :: SX -> Int -> IO (Vector SX)
casadi_vertsplit__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__2" c_casadi_vertsplit__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr SX')))

casadi_vertsplit__2
  :: SX -> Vector Int -> IO (Vector SX)
casadi_vertsplit__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__3" c_casadi_vertsplit__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec (Ptr DM')))

casadi_vertsplit__3
  :: DM -> IO (Vector DM)
casadi_vertsplit__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertsplit__4" c_casadi_vertsplit__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr (StdVec (Ptr DM')))

casadi_vertsplit__4
  :: DM -> Int -> IO (Vector DM)
casadi_vertsplit__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__5" c_casadi_vertsplit__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr DM')))

casadi_vertsplit__5
  :: DM -> Vector Int -> IO (Vector DM)
casadi_vertsplit__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__6" c_casadi_vertsplit__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec (Ptr IM')))

casadi_vertsplit__6
  :: IM -> IO (Vector IM)
casadi_vertsplit__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertsplit__7" c_casadi_vertsplit__7
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr (StdVec (Ptr IM')))

casadi_vertsplit__7
  :: IM -> Int -> IO (Vector IM)
casadi_vertsplit__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__8" c_casadi_vertsplit__8
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr IM')))

casadi_vertsplit__8
  :: IM -> Vector Int -> IO (Vector IM)
casadi_vertsplit__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__9" c_casadi_vertsplit__9
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec (Ptr MX')))

casadi_vertsplit__9
  :: MX -> IO (Vector MX)
casadi_vertsplit__9 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__9 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertsplit__10" c_casadi_vertsplit__10
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi_vertsplit__10
  :: MX -> Int -> IO (Vector MX)
casadi_vertsplit__10 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__10 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__11" c_casadi_vertsplit__11
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr MX')))

casadi_vertsplit__11
  :: MX -> Vector Int -> IO (Vector MX)
casadi_vertsplit__11 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__11 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__12" c_casadi_vertsplit__12
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_vertsplit__12
  :: Sparsity -> IO (Vector Sparsity)
casadi_vertsplit__12 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__12 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "casadi_vertsplit__13" c_casadi_vertsplit__13
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_vertsplit__13
  :: Sparsity -> Int -> IO (Vector Sparsity)
casadi_vertsplit__13 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__13 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_vertsplit__14" c_casadi_vertsplit__14
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr (StdVec CInt) -> IO (Ptr (StdVec (Ptr Sparsity')))

casadi_vertsplit__14
  :: Sparsity -> Vector Int -> IO (Vector Sparsity)
casadi_vertsplit__14 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_vertsplit__14 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "casadi_which_depends__0" c_casadi_which_depends__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> CInt -> IO (Ptr (StdVec CInt))

casadi_which_depends__0
  :: SX -> SX -> Int -> Bool -> IO (Vector Bool)
casadi_which_depends__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_which_depends__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_which_depends__1" c_casadi_which_depends__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> CInt -> IO (Ptr (StdVec CInt))

casadi_which_depends__1
  :: DM -> DM -> Int -> Bool -> IO (Vector Bool)
casadi_which_depends__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_which_depends__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_which_depends__2" c_casadi_which_depends__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> CInt -> IO (Ptr (StdVec CInt))

casadi_which_depends__2
  :: IM -> IM -> Int -> Bool -> IO (Vector Bool)
casadi_which_depends__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_which_depends__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "casadi_which_depends__3" c_casadi_which_depends__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> CInt -> IO (Ptr (StdVec CInt))

casadi_which_depends__3
  :: MX -> MX -> Int -> Bool -> IO (Vector Bool)
casadi_which_depends__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi_which_depends__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "collocation_interpolators" c_collocation_interpolators
  :: Ptr (Ptr StdString) -> Ptr (StdVec CDouble) -> Ptr (Ptr (StdVec (Ptr (StdVec CDouble)))) -> Ptr (Ptr (StdVec CDouble)) -> IO ()

collocation_interpolators
  :: Vector Double -> IO (Vector (Vector Double), Vector Double)
collocation_interpolators x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_collocation_interpolators errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in collocation_interpolators/c_collocation_interpolators" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in collocation_interpolators/c_collocation_interpolators" else wrapReturn o2''

  return (o1''', o2''')



foreign import ccall unsafe "collocation_points__0" c_collocation_points__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr (StdVec CDouble))

collocation_points__0
  :: Int -> IO (Vector Double)
collocation_points__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_collocation_points__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "collocation_points__1" c_collocation_points__1
  :: Ptr (Ptr StdString) -> CInt -> Ptr StdString -> IO (Ptr (StdVec CDouble))

collocation_points__1
  :: Int -> String -> IO (Vector Double)
collocation_points__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_collocation_points__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "complement" c_complement
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> CInt -> IO (Ptr (StdVec CInt))

complement
  :: Vector Int -> Int -> IO (Vector Int)
complement x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_complement errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "conic__0" c_conic__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr Sparsity')) -> IO (Ptr Function')

conic__0
  :: String -> String -> M.Map String Sparsity -> IO Function
conic__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_conic__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "conic__1" c_conic__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr Sparsity')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

conic__1
  :: String -> String -> M.Map String Sparsity -> M.Map String GenericType -> IO Function
conic__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_conic__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "conic_in__0" c_conic_in__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

conic_in__0
  :: Int -> IO String
conic_in__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_conic_in__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "conic_in__1" c_conic_in__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

conic_in__1
  :: IO (Vector String)
conic_in__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_conic_in__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "conic_n_in" c_conic_n_in
  :: Ptr (Ptr StdString) -> IO CInt

conic_n_in
  :: IO Int
conic_n_in  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_conic_n_in errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "conic_n_out" c_conic_n_out
  :: Ptr (Ptr StdString) -> IO CInt

conic_n_out
  :: IO Int
conic_n_out  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_conic_n_out errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "conic_out__0" c_conic_out__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

conic_out__0
  :: Int -> IO String
conic_out__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_conic_out__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "conic_out__1" c_conic_out__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

conic_out__1
  :: IO (Vector String)
conic_out__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_conic_out__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "doc_conic" c_doc_conic
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

doc_conic
  :: String -> IO String
doc_conic x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_doc_conic errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "doc_dple" c_doc_dple
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

doc_dple
  :: String -> IO String
doc_dple x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_doc_dple errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "doc_integrator" c_doc_integrator
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

doc_integrator
  :: String -> IO String
doc_integrator x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_doc_integrator errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "doc_interpolant" c_doc_interpolant
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

doc_interpolant
  :: String -> IO String
doc_interpolant x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_doc_interpolant errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "doc_nlpsol" c_doc_nlpsol
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

doc_nlpsol
  :: String -> IO String
doc_nlpsol x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_doc_nlpsol errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "doc_rootfinder" c_doc_rootfinder
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

doc_rootfinder
  :: String -> IO String
doc_rootfinder x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_doc_rootfinder errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "dple_in__0" c_dple_in__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

dple_in__0
  :: Int -> IO String
dple_in__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_dple_in__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "dple_in__1" c_dple_in__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

dple_in__1
  :: IO (Vector String)
dple_in__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_dple_in__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "dple_n_in" c_dple_n_in
  :: Ptr (Ptr StdString) -> IO CInt

dple_n_in
  :: IO Int
dple_n_in  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_dple_n_in errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "dple_n_out" c_dple_n_out
  :: Ptr (Ptr StdString) -> IO CInt

dple_n_out
  :: IO Int
dple_n_out  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_dple_n_out errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "dple_out__0" c_dple_out__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

dple_out__0
  :: Int -> IO String
dple_out__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_dple_out__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "dple_out__1" c_dple_out__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

dple_out__1
  :: IO (Vector String)
dple_out__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_dple_out__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "dplesol__0" c_dplesol__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr StdString -> IO (Ptr (StdVec (Ptr DM')))

dplesol__0
  :: Vector DM -> Vector DM -> String -> IO (Vector DM)
dplesol__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "dplesol__1" c_dplesol__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr DM')) -> Ptr (StdVec (Ptr DM')) -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr DM')))

dplesol__1
  :: Vector DM -> Vector DM -> String -> M.Map String GenericType -> IO (Vector DM)
dplesol__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "dplesol__2" c_dplesol__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr StdString -> IO (Ptr (StdVec (Ptr MX')))

dplesol__2
  :: Vector MX -> Vector MX -> String -> IO (Vector MX)
dplesol__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "dplesol__3" c_dplesol__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec (Ptr MX')))

dplesol__3
  :: Vector MX -> Vector MX -> String -> M.Map String GenericType -> IO (Vector MX)
dplesol__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "dplesol__4" c_dplesol__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr StdString -> IO (Ptr MX')

dplesol__4
  :: MX -> MX -> String -> IO MX
dplesol__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "dplesol__5" c_dplesol__5
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr MX')

dplesol__5
  :: MX -> MX -> String -> M.Map String GenericType -> IO MX
dplesol__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "dplesol__6" c_dplesol__6
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr Sparsity')) -> IO (Ptr Function')

dplesol__6
  :: String -> String -> M.Map String Sparsity -> IO Function
dplesol__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "dplesol__7" c_dplesol__7
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr Sparsity')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

dplesol__7
  :: String -> String -> M.Map String Sparsity -> M.Map String GenericType -> IO Function
dplesol__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_dplesol__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "external__0" c_external__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Importer' -> IO (Ptr Function')

external__0
  :: String -> Importer -> IO Function
external__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_external__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "external__1" c_external__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Importer' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

external__1
  :: String -> Importer -> M.Map String GenericType -> IO Function
external__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_external__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "external__2" c_external__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> IO (Ptr Function')

external__2
  :: String -> String -> IO Function
external__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_external__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "external__3" c_external__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

external__3
  :: String -> String -> M.Map String GenericType -> IO Function
external__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_external__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "external__4" c_external__4
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr Function')

external__4
  :: String -> IO Function
external__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_external__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "external__5" c_external__5
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

external__5
  :: String -> M.Map String GenericType -> IO Function
external__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_external__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "has_conic" c_has_conic
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

has_conic
  :: String -> IO Bool
has_conic x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_has_conic errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "has_dple" c_has_dple
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

has_dple
  :: String -> IO Bool
has_dple x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_has_dple errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "has_integrator" c_has_integrator
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

has_integrator
  :: String -> IO Bool
has_integrator x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_has_integrator errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "has_interpolant" c_has_interpolant
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

has_interpolant
  :: String -> IO Bool
has_interpolant x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_has_interpolant errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "has_nlpsol" c_has_nlpsol
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

has_nlpsol
  :: String -> IO Bool
has_nlpsol x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_has_nlpsol errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "has_rootfinder" c_has_rootfinder
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

has_rootfinder
  :: String -> IO Bool
has_rootfinder x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_has_rootfinder errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "hash_combine" c_hash_combine
  :: Ptr (Ptr StdString) -> CSize -> Ptr (StdVec CInt) -> IO ()

hash_combine
  :: CSize -> Vector Int -> IO ()
hash_combine x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_hash_combine errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



foreign import ccall unsafe "hash_sparsity" c_hash_sparsity
  :: Ptr (Ptr StdString) -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO CSize

hash_sparsity
  :: Int -> Int -> Vector Int -> Vector Int -> IO CSize
hash_sparsity x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_hash_sparsity errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "integrator__0" c_integrator__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> IO (Ptr Function')

integrator__0
  :: String -> String -> M.Map String MX -> IO Function
integrator__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_integrator__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "integrator__1" c_integrator__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

integrator__1
  :: String -> String -> M.Map String MX -> M.Map String GenericType -> IO Function
integrator__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_integrator__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "integrator__2" c_integrator__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> IO (Ptr Function')

integrator__2
  :: String -> String -> M.Map String SX -> IO Function
integrator__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_integrator__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "integrator__3" c_integrator__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

integrator__3
  :: String -> String -> M.Map String SX -> M.Map String GenericType -> IO Function
integrator__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_integrator__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "integrator_in__0" c_integrator_in__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

integrator_in__0
  :: Int -> IO String
integrator_in__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_integrator_in__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "integrator_in__1" c_integrator_in__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

integrator_in__1
  :: IO (Vector String)
integrator_in__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_integrator_in__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "integrator_n_in" c_integrator_n_in
  :: Ptr (Ptr StdString) -> IO CInt

integrator_n_in
  :: IO Int
integrator_n_in  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_integrator_n_in errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "integrator_n_out" c_integrator_n_out
  :: Ptr (Ptr StdString) -> IO CInt

integrator_n_out
  :: IO Int
integrator_n_out  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_integrator_n_out errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "integrator_out__0" c_integrator_out__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

integrator_out__0
  :: Int -> IO String
integrator_out__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_integrator_out__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "integrator_out__1" c_integrator_out__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

integrator_out__1
  :: IO (Vector String)
integrator_out__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_integrator_out__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "interpolant__0" c_interpolant__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdVec (Ptr (StdVec CDouble))) -> Ptr (StdVec CDouble) -> IO (Ptr Function')

interpolant__0
  :: String -> String -> Vector (Vector Double) -> Vector Double -> IO Function
interpolant__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_interpolant__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "interpolant__1" c_interpolant__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdVec (Ptr (StdVec CDouble))) -> Ptr (StdVec CDouble) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

interpolant__1
  :: String -> String -> Vector (Vector Double) -> Vector Double -> M.Map String GenericType -> IO Function
interpolant__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_interpolant__1 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "is_slice__0" c_is_slice__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

is_slice__0
  :: IM -> IO Bool
is_slice__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_is_slice__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "is_slice__1" c_is_slice__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO CInt

is_slice__1
  :: IM -> Bool -> IO Bool
is_slice__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_is_slice__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "is_slice__2" c_is_slice__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> IO CInt

is_slice__2
  :: Vector Int -> IO Bool
is_slice__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_is_slice__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "is_slice__3" c_is_slice__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> CInt -> IO CInt

is_slice__3
  :: Vector Int -> Bool -> IO Bool
is_slice__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_is_slice__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "is_slice2" c_is_slice2
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> IO CInt

is_slice2
  :: Vector Int -> IO Bool
is_slice2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_is_slice2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "jit__0" c_jit__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> Ptr StdString -> IO (Ptr Function')

jit__0
  :: String -> Int -> Int -> String -> IO Function
jit__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_jit__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "jit__1" c_jit__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

jit__1
  :: String -> Int -> Int -> String -> M.Map String GenericType -> IO Function
jit__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_jit__1 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "load_conic" c_load_conic
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

load_conic
  :: String -> IO ()
load_conic x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_load_conic errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



foreign import ccall unsafe "load_dple" c_load_dple
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

load_dple
  :: String -> IO ()
load_dple x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_load_dple errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



foreign import ccall unsafe "load_integrator" c_load_integrator
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

load_integrator
  :: String -> IO ()
load_integrator x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_load_integrator errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



foreign import ccall unsafe "load_interpolant" c_load_interpolant
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

load_interpolant
  :: String -> IO ()
load_interpolant x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_load_interpolant errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



foreign import ccall unsafe "load_nlpsol" c_load_nlpsol
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

load_nlpsol
  :: String -> IO ()
load_nlpsol x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_load_nlpsol errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



foreign import ccall unsafe "load_rootfinder" c_load_rootfinder
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

load_rootfinder
  :: String -> IO ()
load_rootfinder x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_load_rootfinder errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



foreign import ccall unsafe "lookupvector" c_lookupvector
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> CInt -> IO (Ptr (StdVec CInt))

lookupvector
  :: Vector Int -> Int -> IO (Vector Int)
lookupvector x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_lookupvector errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "nlpsol__0" c_nlpsol__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr NlpBuilder' -> IO (Ptr Function')

nlpsol__0
  :: String -> String -> NlpBuilder -> IO Function
nlpsol__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "nlpsol__1" c_nlpsol__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr NlpBuilder' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

nlpsol__1
  :: String -> String -> NlpBuilder -> M.Map String GenericType -> IO Function
nlpsol__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "nlpsol__2" c_nlpsol__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr Importer' -> IO (Ptr Function')

nlpsol__2
  :: String -> String -> Importer -> IO Function
nlpsol__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "nlpsol__3" c_nlpsol__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr Importer' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

nlpsol__3
  :: String -> String -> Importer -> M.Map String GenericType -> IO Function
nlpsol__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "nlpsol__4" c_nlpsol__4
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr StdString -> IO (Ptr Function')

nlpsol__4
  :: String -> String -> String -> IO Function
nlpsol__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "nlpsol__5" c_nlpsol__5
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

nlpsol__5
  :: String -> String -> String -> M.Map String GenericType -> IO Function
nlpsol__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "nlpsol__6" c_nlpsol__6
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> IO (Ptr Function')

nlpsol__6
  :: String -> String -> M.Map String MX -> IO Function
nlpsol__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "nlpsol__7" c_nlpsol__7
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

nlpsol__7
  :: String -> String -> M.Map String MX -> M.Map String GenericType -> IO Function
nlpsol__7 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__7 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "nlpsol__8" c_nlpsol__8
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> IO (Ptr Function')

nlpsol__8
  :: String -> String -> M.Map String SX -> IO Function
nlpsol__8 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__8 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "nlpsol__9" c_nlpsol__9
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

nlpsol__9
  :: String -> String -> M.Map String SX -> M.Map String GenericType -> IO Function
nlpsol__9 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol__9 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "nlpsol_default_in__0" c_nlpsol_default_in__0
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec CDouble))

nlpsol_default_in__0
  :: IO (Vector Double)
nlpsol_default_in__0  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_default_in__0 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "nlpsol_default_in__1" c_nlpsol_default_in__1
  :: Ptr (Ptr StdString) -> CInt -> IO CDouble

nlpsol_default_in__1
  :: Int -> IO Double
nlpsol_default_in__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_default_in__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "nlpsol_in__0" c_nlpsol_in__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

nlpsol_in__0
  :: Int -> IO String
nlpsol_in__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_in__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "nlpsol_in__1" c_nlpsol_in__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

nlpsol_in__1
  :: IO (Vector String)
nlpsol_in__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_in__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "nlpsol_n_in" c_nlpsol_n_in
  :: Ptr (Ptr StdString) -> IO CInt

nlpsol_n_in
  :: IO Int
nlpsol_n_in  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_n_in errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "nlpsol_n_out" c_nlpsol_n_out
  :: Ptr (Ptr StdString) -> IO CInt

nlpsol_n_out
  :: IO Int
nlpsol_n_out  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_n_out errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "nlpsol_out__0" c_nlpsol_out__0
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr StdString)

nlpsol_out__0
  :: Int -> IO String
nlpsol_out__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_out__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "nlpsol_out__1" c_nlpsol_out__1
  :: Ptr (Ptr StdString) -> IO (Ptr (StdVec (Ptr StdString)))

nlpsol_out__1
  :: IO (Vector String)
nlpsol_out__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_nlpsol_out__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



foreign import ccall unsafe "qpsol__0" c_qpsol__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> IO (Ptr Function')

qpsol__0
  :: String -> String -> M.Map String MX -> IO Function
qpsol__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_qpsol__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "qpsol__1" c_qpsol__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

qpsol__1
  :: String -> String -> M.Map String MX -> M.Map String GenericType -> IO Function
qpsol__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_qpsol__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "qpsol__2" c_qpsol__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> IO (Ptr Function')

qpsol__2
  :: String -> String -> M.Map String SX -> IO Function
qpsol__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_qpsol__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "qpsol__3" c_qpsol__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

qpsol__3
  :: String -> String -> M.Map String SX -> M.Map String GenericType -> IO Function
qpsol__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_qpsol__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "rootfinder__0" c_rootfinder__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr Function' -> IO (Ptr Function')

rootfinder__0
  :: String -> String -> Function -> IO Function
rootfinder__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_rootfinder__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "rootfinder__1" c_rootfinder__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr Function' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

rootfinder__1
  :: String -> String -> Function -> M.Map String GenericType -> IO Function
rootfinder__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_rootfinder__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "simpleIRK__0" c_simpleIRK__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

simpleIRK__0
  :: Function -> IO Function
simpleIRK__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIRK__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "simpleIRK__1" c_simpleIRK__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Function')

simpleIRK__1
  :: Function -> Int -> IO Function
simpleIRK__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIRK__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "simpleIRK__2" c_simpleIRK__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> IO (Ptr Function')

simpleIRK__2
  :: Function -> Int -> Int -> IO Function
simpleIRK__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIRK__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "simpleIRK__3" c_simpleIRK__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> Ptr StdString -> IO (Ptr Function')

simpleIRK__3
  :: Function -> Int -> Int -> String -> IO Function
simpleIRK__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIRK__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



foreign import ccall unsafe "simpleIRK__4" c_simpleIRK__4
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> Ptr StdString -> Ptr StdString -> IO (Ptr Function')

simpleIRK__4
  :: Function -> Int -> Int -> String -> String -> IO Function
simpleIRK__4 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIRK__4 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



foreign import ccall unsafe "simpleIRK__5" c_simpleIRK__5
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

simpleIRK__5
  :: Function -> Int -> Int -> String -> String -> M.Map String GenericType -> IO Function
simpleIRK__5 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIRK__5 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



foreign import ccall unsafe "simpleIntegrator__0" c_simpleIntegrator__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

simpleIntegrator__0
  :: Function -> IO Function
simpleIntegrator__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIntegrator__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "simpleIntegrator__1" c_simpleIntegrator__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Function')

simpleIntegrator__1
  :: Function -> String -> IO Function
simpleIntegrator__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIntegrator__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "simpleIntegrator__2" c_simpleIntegrator__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

simpleIntegrator__2
  :: Function -> String -> M.Map String GenericType -> IO Function
simpleIntegrator__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleIntegrator__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "simpleRK__0" c_simpleRK__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

simpleRK__0
  :: Function -> IO Function
simpleRK__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleRK__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "simpleRK__1" c_simpleRK__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Function')

simpleRK__1
  :: Function -> Int -> IO Function
simpleRK__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleRK__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "simpleRK__2" c_simpleRK__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> IO (Ptr Function')

simpleRK__2
  :: Function -> Int -> Int -> IO Function
simpleRK__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_simpleRK__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



foreign import ccall unsafe "to_slice__0" c_to_slice__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr Slice')

to_slice__0
  :: IM -> IO Slice
to_slice__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_to_slice__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "to_slice__1" c_to_slice__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr Slice')

to_slice__1
  :: IM -> Bool -> IO Slice
to_slice__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_to_slice__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "to_slice__2" c_to_slice__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> IO (Ptr Slice')

to_slice__2
  :: Vector Int -> IO Slice
to_slice__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_to_slice__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



foreign import ccall unsafe "to_slice__3" c_to_slice__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> CInt -> IO (Ptr Slice')

to_slice__3
  :: Vector Int -> Bool -> IO Slice
to_slice__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_to_slice__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



foreign import ccall unsafe "to_slice2" c_to_slice2
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> IO (Ptr (StdPair (Ptr Slice') (Ptr Slice')))

to_slice2
  :: Vector Int -> IO (Slice, Slice)
to_slice2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_to_slice2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



