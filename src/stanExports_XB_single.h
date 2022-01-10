// Generated by rstantools.  Do not edit by hand.

/*
    slopeflex is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    slopeflex is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with slopeflex.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_XB_single_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_XB_single");
    reader.add_event(39, 37, "end", "model_XB_single");
    return reader;
}
#include <stan_meta_header.hpp>
class model_XB_single
  : public stan::model::model_base_crtp<model_XB_single> {
private:
        int n;
        int m;
        int k;
        int h;
        double a;
        double a_sd;
        double b;
        double b_sd;
        vector_d y;
        matrix_d X;
        matrix_d X_fc;
        double params;
        double params_sd;
        vector_d yy;
public:
    model_XB_single(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_XB_single(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_XB_single_namespace::model_XB_single";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 2;
            context__.validate_dims("data initialization", "n", "int", context__.to_vec());
            n = int(0);
            vals_i__ = context__.vals_i("n");
            pos__ = 0;
            n = vals_i__[pos__++];
            check_greater_or_equal(function__, "n", n, 1);
            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "m", "int", context__.to_vec());
            m = int(0);
            vals_i__ = context__.vals_i("m");
            pos__ = 0;
            m = vals_i__[pos__++];
            check_greater_or_equal(function__, "m", m, 2);
            current_statement_begin__ = 4;
            context__.validate_dims("data initialization", "k", "int", context__.to_vec());
            k = int(0);
            vals_i__ = context__.vals_i("k");
            pos__ = 0;
            k = vals_i__[pos__++];
            check_greater_or_equal(function__, "k", k, 1);
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "h", "int", context__.to_vec());
            h = int(0);
            vals_i__ = context__.vals_i("h");
            pos__ = 0;
            h = vals_i__[pos__++];
            check_greater_or_equal(function__, "h", h, 1);
            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "a", "double", context__.to_vec());
            a = double(0);
            vals_r__ = context__.vals_r("a");
            pos__ = 0;
            a = vals_r__[pos__++];
            current_statement_begin__ = 8;
            context__.validate_dims("data initialization", "a_sd", "double", context__.to_vec());
            a_sd = double(0);
            vals_r__ = context__.vals_r("a_sd");
            pos__ = 0;
            a_sd = vals_r__[pos__++];
            check_greater_or_equal(function__, "a_sd", a_sd, 0);
            current_statement_begin__ = 9;
            context__.validate_dims("data initialization", "b", "double", context__.to_vec());
            b = double(0);
            vals_r__ = context__.vals_r("b");
            pos__ = 0;
            b = vals_r__[pos__++];
            current_statement_begin__ = 10;
            context__.validate_dims("data initialization", "b_sd", "double", context__.to_vec());
            b_sd = double(0);
            vals_r__ = context__.vals_r("b_sd");
            pos__ = 0;
            b_sd = vals_r__[pos__++];
            check_greater_or_equal(function__, "b_sd", b_sd, 0);
            current_statement_begin__ = 11;
            validate_non_negative_index("y", "n", n);
            context__.validate_dims("data initialization", "y", "vector_d", context__.to_vec(n));
            y = Eigen::Matrix<double, Eigen::Dynamic, 1>(n);
            vals_r__ = context__.vals_r("y");
            pos__ = 0;
            size_t y_j_1_max__ = n;
            for (size_t j_1__ = 0; j_1__ < y_j_1_max__; ++j_1__) {
                y(j_1__) = vals_r__[pos__++];
            }
            current_statement_begin__ = 12;
            validate_non_negative_index("X", "n", n);
            validate_non_negative_index("X", "m", m);
            context__.validate_dims("data initialization", "X", "matrix_d", context__.to_vec(n,m));
            X = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(n, m);
            vals_r__ = context__.vals_r("X");
            pos__ = 0;
            size_t X_j_2_max__ = m;
            size_t X_j_1_max__ = n;
            for (size_t j_2__ = 0; j_2__ < X_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < X_j_1_max__; ++j_1__) {
                    X(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 13;
            validate_non_negative_index("X_fc", "h", h);
            validate_non_negative_index("X_fc", "m", m);
            context__.validate_dims("data initialization", "X_fc", "matrix_d", context__.to_vec(h,m));
            X_fc = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(h, m);
            vals_r__ = context__.vals_r("X_fc");
            pos__ = 0;
            size_t X_fc_j_2_max__ = m;
            size_t X_fc_j_1_max__ = h;
            for (size_t j_2__ = 0; j_2__ < X_fc_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < X_fc_j_1_max__; ++j_1__) {
                    X_fc(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 14;
            context__.validate_dims("data initialization", "params", "double", context__.to_vec());
            params = double(0);
            vals_r__ = context__.vals_r("params");
            pos__ = 0;
            params = vals_r__[pos__++];
            current_statement_begin__ = 15;
            context__.validate_dims("data initialization", "params_sd", "double", context__.to_vec());
            params_sd = double(0);
            vals_r__ = context__.vals_r("params_sd");
            pos__ = 0;
            params_sd = vals_r__[pos__++];
            check_greater_or_equal(function__, "params_sd", params_sd, 0);
            // initialize transformed data variables
            current_statement_begin__ = 18;
            validate_non_negative_index("yy", "n", n);
            yy = Eigen::Matrix<double, Eigen::Dynamic, 1>(n);
            stan::math::fill(yy, DUMMY_VAR__);
            // execute transformed data statements
            current_statement_begin__ = 19;
            stan::math::assign(yy, subtract(y, get_base1(y, 1, "y", 1)));
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 22;
            validate_non_negative_index("B", "m", m);
            num_params_r__ += m;
            current_statement_begin__ = 23;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_XB_single() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 22;
        if (!(context__.contains_r("B")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable B missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("B");
        pos__ = 0U;
        validate_non_negative_index("B", "m", m);
        context__.validate_dims("parameter initialization", "B", "vector_d", context__.to_vec(m));
        Eigen::Matrix<double, Eigen::Dynamic, 1> B(m);
        size_t B_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < B_j_1_max__; ++j_1__) {
            B(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(B);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable B: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 23;
        if (!(context__.contains_r("sig")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sig missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sig");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sig", "double", context__.to_vec());
        double sig(0);
        sig = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sig);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sig: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 22;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> B;
            (void) B;  // dummy to suppress unused var warning
            if (jacobian__)
                B = in__.vector_constrain(m, lp__);
            else
                B = in__.vector_constrain(m);
            current_statement_begin__ = 23;
            local_scalar_t__ sig;
            (void) sig;  // dummy to suppress unused var warning
            if (jacobian__)
                sig = in__.scalar_lb_constrain(0, lp__);
            else
                sig = in__.scalar_lb_constrain(0);
            // model body
            current_statement_begin__ = 26;
            lp_accum__.add(normal_log<propto__>(get_base1(B, 1, "B", 1), a, a_sd));
            current_statement_begin__ = 27;
            lp_accum__.add(normal_log<propto__>(get_base1(B, 2, "B", 1), b, b_sd));
            current_statement_begin__ = 28;
            lp_accum__.add(normal_log<propto__>(get_base1(B, 3, "B", 1), params, params_sd));
            current_statement_begin__ = 29;
            lp_accum__.add(exponential_log<propto__>(sig, 1));
            current_statement_begin__ = 30;
            lp_accum__.add(normal_log<propto__>(yy, multiply(X, B), sig));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("B");
        names__.push_back("sig");
        names__.push_back("yhat");
        names__.push_back("yhat_fc");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(m);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(n);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back((n + h));
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_XB_single_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        Eigen::Matrix<double, Eigen::Dynamic, 1> B = in__.vector_constrain(m);
        size_t B_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < B_j_1_max__; ++j_1__) {
            vars__.push_back(B(j_1__));
        }
        double sig = in__.scalar_lb_constrain(0);
        vars__.push_back(sig);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
            // declare and define generated quantities
            current_statement_begin__ = 33;
            validate_non_negative_index("yhat", "n", n);
            Eigen::Matrix<double, Eigen::Dynamic, 1> yhat(n);
            stan::math::initialize(yhat, DUMMY_VAR__);
            stan::math::fill(yhat, DUMMY_VAR__);
            current_statement_begin__ = 34;
            validate_non_negative_index("yhat_fc", "(n + h)", (n + h));
            Eigen::Matrix<double, Eigen::Dynamic, 1> yhat_fc((n + h));
            stan::math::initialize(yhat_fc, DUMMY_VAR__);
            stan::math::fill(yhat_fc, DUMMY_VAR__);
            // generated quantities statements
            current_statement_begin__ = 35;
            stan::math::assign(yhat, add(multiply(X, B), get_base1(y, 1, "y", 1)));
            current_statement_begin__ = 36;
            stan::math::assign(yhat_fc, add(multiply(append_row(X, X_fc), B), get_base1(y, 1, "y", 1)));
            // validate, write generated quantities
            current_statement_begin__ = 33;
            size_t yhat_j_1_max__ = n;
            for (size_t j_1__ = 0; j_1__ < yhat_j_1_max__; ++j_1__) {
                vars__.push_back(yhat(j_1__));
            }
            current_statement_begin__ = 34;
            size_t yhat_fc_j_1_max__ = (n + h);
            for (size_t j_1__ = 0; j_1__ < yhat_fc_j_1_max__; ++j_1__) {
                vars__.push_back(yhat_fc(j_1__));
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_XB_single";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t B_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < B_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "B" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sig";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
        size_t yhat_j_1_max__ = n;
        for (size_t j_1__ = 0; j_1__ < yhat_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "yhat" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t yhat_fc_j_1_max__ = (n + h);
        for (size_t j_1__ = 0; j_1__ < yhat_fc_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "yhat_fc" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t B_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < B_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "B" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sig";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
        size_t yhat_j_1_max__ = n;
        for (size_t j_1__ = 0; j_1__ < yhat_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "yhat" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t yhat_fc_j_1_max__ = (n + h);
        for (size_t j_1__ = 0; j_1__ < yhat_fc_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "yhat_fc" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
}; // model
}  // namespace
typedef model_XB_single_namespace::model_XB_single stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif