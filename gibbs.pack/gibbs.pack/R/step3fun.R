#' step3fun
#'
#' Runs the third step of the Gibbs sampler, which samples \theta ideal point distribution given the funciton's inputs. The returned value is a number.
#'
#' @param sigma A number, ?
#' @param beta A 2x1 matrix, the estimate of beta sampled in step 2
#' @param f_prior A number, ?
#' @param y A 2x1 matrix, ?
#'
#' @return A number \theta_i^{(t)}:
#'  \item{\theta_i^{(t)}}{The sampled \theta from the distribution}
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note Need some clarity on the other values
#' @examples
#'
#'
#' @seealso
#' @aliases step3
#' @rdname step3fun
#' @import
#' @export

#set the generic for the function
setGeneric(name = "step3fun",
           def = function(sigma, beta, f_prior, y)
           {standardGeneric("step3fun")}
)

#Define the method
setMethod(f = "step3fun",
          definition = function(sigma, beta, f_prior, y){
            # Applying the definition of v_theta
            #betas are current time, t, betas
            v_theta <- solve( (1 / sigma) + (t(beta) %*% beta) )

            # Applying the definition of m_theta
            #beta is current time,t, beta
            #f_prior is the prior time's f, f^(t-1)
            m_theta <- v_theta %*% ((f_prior %/% sigma) + (t(beta) %*% y))

            # Attempt at sampling theta from multivariate normal distribution
            #Note the Sigma here is not the same as the sigma argument
            theta_sample <- MASS::mvrnorm(n, mu = m_theta, Sigma = v_theta)

            return(theta_sample)
            }
)
