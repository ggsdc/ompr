#' Get all unique names of the model variables
#'
#' @param model the model
#' @return a character vector ordered in the same way
#'         as the constraint matrix columns and objective vector
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3)
#' variable_keys(model)
#' @export
variable_keys <- function(model) UseMethod("variable_keys")

#' @export
variable_keys.optimization_model <- function(model) {
  if (length(model$variables) == 0) {
    return(character(0))
  }
  sort(unlist(lapply(sort(names(model$variables)),
    function(x) {
      var <- model$variables[[x]]
      if (var$arity > 0) {
        var_codes <- paste0(x, "_", var$instances)
        vapply(var_codes, function(var_code) {
          splited_els <- strsplit(var_code, "_", fixed = TRUE)[[1]]
          paste0(splited_els[1], "[",
                 paste0(splited_els[2:length(splited_els)], collapse = ","),
                 "]")
        }, character(1))
      } else {
        x
      }
    }), use.names = FALSE)
  )
}

# helper function that creates a function
# that can extract coefficients and constants
# out of an expression
build_coefficent_vector_fun <- function(model_var_keys) {
  n_cols <- length(model_var_keys)
  function(extracted_coefficients) {
    coef_vector <- rep.int(0, n_cols)
    coefficients <- extracted_coefficients
    names(coefficients) <- NULL
    bound_coefs <- unlist(Map(function(var_coef) {
      var_ast <- var_coef$ast
      if (is.call(var_ast) && length(var_ast) > 1) {
        var_name <- as.character(var_ast[[2]])
        search_key <- paste0(var_name, "[",
                             paste0(as.character(var_ast[3:length(var_ast)]),
                                    collapse = ","), "]")
      } else {
        var_name <- as.character(var_ast)
        search_key <- var_name
      }
      setNames(var_coef$coef, search_key)
    }, coefficients))
    coef_positions <- match(names(bound_coefs), model_var_keys)
    coef_positions <- coef_positions[!is.na(coef_positions)]
    if (length(coef_positions) > 0) {
      coef_vector[coef_positions] <- as.numeric(bound_coefs)
    }
    coef_vector
  }
}


#' Extract the objective function from a model
#'
#' @param model the model
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   set_objective(sum_expr(i * x[i], i = 1:5) + 10)
#' objective_function(model)
#' @export
objective_function <- function(model) UseMethod("objective_function")

#' @export
objective_function.optimization_model <- function(model) {
  objective <- model$objective
  has_objective <- !is.null(objective)
  build_coefficent_vector <- build_coefficent_vector_fun(variable_keys(model))
  if (has_objective) {
    coefficients <- extract_coefficients_internal(
      model$objective$expression[[1]])
    obj_constant <- coefficients$constant
    if (!is.numeric(obj_constant)) obj_constant <- 0
    coefficients <- coefficients$coefficients
    names(coefficients) <- NULL
    obj_vector <- build_coefficent_vector(coefficients)
    list(vector = obj_vector, constant = obj_constant)
  } else {
    n_vars <- sum(unlist(nvars(model)))
    list(vector = rep.int(0, n_vars), constant = 0)
  }
}

#' Extract the constraint matrix, the right hand side and the directions from a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'matrix' is the constraint matrix.
#'         'rhs' is the right hand side vector in the order of the matrix.
#'         'direction' is a vector of the constraint directions
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3) %>%
#'   add_variable(y[i], i = 1:3) %>%
#'   add_constraint(x[i] + y[i] <= 1, i = 1:3)
#' extract_constraints(model)
#' @export
extract_constraints <- function(model) UseMethod("extract_constraints")

#' @export
extract_constraints.optimization_model <- function(model) {
  build_coefficent_vector <- build_coefficent_vector_fun(variable_keys(model))
  matrices <- lapply(model$constraints, function(constraint) {
    coefficients_lhs <- extract_coefficients_internal(constraint$lhs[[1]])
    coefficients_rhs <- extract_coefficients_internal(constraint$rhs[[1]])
    direction <- constraint$direction
    list(
      lhs = build_coefficent_vector(coefficients_lhs$coefficients),
      rhs = build_coefficent_vector(coefficients_rhs$coefficients),
      direction = direction,
      lhs_constant = coefficients_lhs$constant,
      rhs_constant = coefficients_rhs$constant
    )
  })

  constraint_matrix <- t(rbind(sapply(matrices, function(constraint) {
    constraint$lhs - constraint$rhs
  })))

  # build row upper bound (aka b)
  constraint_rhs <- vapply(matrices, function(constraint) {
    constraint$rhs_constant - constraint$lhs_constant
  }, numeric(1))

  constraint_dir <- vapply(matrices, function(constraint) {
    constraint$direction
  }, character(1))

  list(
    matrix = constraint_matrix,
    direction = constraint_dir,
    rhs = constraint_rhs
  )
}

#' Number of variables of a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'binary' => number of binary variables,
#'         'integer' => number of integer variables,
#'         'continuous' => number of continuous variables.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:10, type = "binary") %>%
#'   add_variable(y[i], i = 1:5, type = "continuous") %>%
#'   add_variable(z[i], i = 1:2, type = "integer")
#' nvars(model)
#' @export
nvars <- function(model) UseMethod("nvars")

#' @export
nvars.optimization_model <- function(model) {
  stopifnot(is.list(model$variables))
  mapped_vars <- Map(f = function(var) {
    setNames(length(var$instances), var$type)
  }, model$variables)
  Reduce(f = function(acc, el) {
    acc[[names(el)]] <- acc[[names(el)]] + as.numeric(el)
    acc
  }, mapped_vars, init = list(continuous = 0, integer = 0,
                              binary = 0))
}

#' Variable types of a model
#'
#' One component for each variable in the correct order
#' @param model the model
#' @return a factor with levels binary, continuous, integer
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous") %>%
#'   add_variable(z, type = "integer")
#' variable_types(model)
#' @export
variable_types <- function(model) UseMethod("variable_types")

#' @export
variable_types.optimization_model <- function(model) {
  vars <- model$variables
  if (length(vars) == 0) {
    return(factor())
  }
  factor(unlist(lapply(sort(names(model$variables)), function(key) {
    var <- vars[[key]]
    rep.int(x = var$type, times = length(var$instances))
  })))
}

# a function to extract the lower/upper bounds of variables
extract_var_bounds_fun <- function(type) {
  stopifnot(type %in% c("lb", "ub"))
  default_val <- if (type == "lb") -Inf else Inf
  function(variables, keys) {
    unlist(lapply(keys, function(key) {
      var <- variables[[key]]
      bound <- if (length(var[[type]]) == 0) default_val else var[[type]]
      is_binary_var <- length(var[[type]]) == 0 && var$type == "binary"
      if (is_binary_var && type == "lb") bound <- 0
      if (is_binary_var && type == "ub") bound <- 1
      bound
    }))
  }
}

#' Variable lower and upper bounds of a model
#'
#' @param model the model
#'
#' @return a list with two components 'lower' and 'upper' each
#' having a numeric vector of bounds. One for each variable.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous", lb = 2) %>%
#'   add_variable(z, type = "integer", ub = 3)
#' variable_bounds(model)
#' @export
variable_bounds <- function(model) UseMethod("variable_bounds")

#' @export
variable_bounds.optimization_model <- function(model) {
  model_vars <- model$variables
  if (length(model_vars) == 0) {
    return(list(lower = numeric(0), upper = numeric(0)))
  }
  extract_bounds_l <- extract_var_bounds_fun("lb")
  extract_bounds_u <- extract_var_bounds_fun("ub")
  keys <- sort(names(model$variables))
  list(
    lower = extract_bounds_l(model_vars, keys),
    upper = extract_bounds_u(model_vars, keys)
  )
}

#' Write the model as a .mps file
#'
#' @param model the model
#' @param file the route and name of the output file
#' @param modelname (optional). Sets the model name inside the file, default is "OMPR1"
#'
#' @return It writes the model to the indicated file
#'
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous", lb = 2) %>%
#'   add_variable(z, type = "integer", ub = 3)
#'
#' write_MPS(model = model, file = "test.mps", modelname = "TEST")
#' @export
write_MPS <- function(model, file, modelname="OMPR1") UseMethod("write_MPS")

#' @export
write_MPS.optimization_model <- function(model, file, modelname="OMPR1"){
  # Control section
  Rprof()

  # Prepare MPS data format
  mpsConsTypes <- c("E", "L", "G")
  consTypes <- c("==", "<=", ">=")

  mpsUpperBoundType <- c("UI", "BV", "UP")
  mpsLowBoundType <- c("LI", "", "LO")
  omprBoundType <- c("integer", "binary", "continuous")


  # Prepare model data section
  modelConstraints <- extract_constraints(model)
  modelVarNames <- variable_keys(model)
  modelObjFunction <- objective_function(model)
  modelVarType <- as.character(variable_types(model))
  modelVarBounds <- variable_bounds(model)

  matrixA <- modelConstraints$matrix
  vectorC <- modelObjFunction$vector

  numVar <- ncol(matrixA)
  namesVars <- str_c("V_", formatC(seq(1:numVar), width=nchar(numVar), format="d", flag="0"))
  widthVars <- 2 + nchar(numVar)

  numCons <- nrow(matrixA)
  namesCons <- str_c("R_", formatC(seq(1:numCons), width=nchar(numCons), format="d", flag="0"))
  widthCons <- 2 + nchar(numCons)

  colnames(matrixA) <- namesVars
  rownames(matrixA) <- namesCons

  dirCons <- modelConstraints$direction
  rhsCons <- modelConstraints$rhs

  dfCons <- data.frame(namesCons,
                       dirCons,
                       rhsCons,
                       stringsAsFactors=FALSE) %>%
    rename(cons = namesCons,
           dir = dirCons,
           rhs = rhsCons) %>%
    rowwise() %>%
    mutate(mpsDir = mgsub(dir, consTypes, mpsConsTypes)) %>%
    mutate(row = str_c(str_pad("", 1, pad=" ", side="left"),
                       mpsDir,
                       str_pad("",2,pad=" ", side="left"),
                       cons)) %>%
    mutate(rhsLine = str_c(str_pad(string = "", width =4, side = "left", pad = " "),
                           "RHS1",
                           str_pad(string = "", width = (10 - nchar("RHS1")), side = "left", pad = " "),
                           cons,
                           str_pad(string="", width = (10 - nchar(cons)), side = "left", pad = " " ),
                           formatC(rhs, digits = 10, format="g"))) %>%
    data.table()

  dfVectorC <- data.frame("OBJ",
                          namesVars,
                          vectorC,
                          stringsAsFactors = FALSE) %>%
    rename(cons= X.OBJ., vars=namesVars, coef=vectorC)


  dfMatrixA <- as.data.frame(as.table(matrixA), stringsAsFactors = FALSE) %>%
    rename(cons=Var1, vars=Var2, coef=Freq) %>%
    rbind(dfVectorC) %>%
    filter(coef!=0) %>%
    arrange(vars, cons) %>%
    rowwise() %>%
    mutate(line = str_c(str_pad(string = "", width =4, side = "left", pad = " "),
                        vars,
                        str_pad(string = "", width = (10 - nchar(vars)), side = "left", pad = " "),
                        cons,
                        str_pad(string="", width = (10 - nchar(cons)), side = "left", pad = " " ),
                        formatC(coef, digits = 10, format="g"))) %>%
    data.table()

  dfVarBoundUp <-data.frame(modelVarType,
                            namesVars,
                            "UPPER",
                            modelVarBounds$upper,
                            stringsAsFactors = FALSE) %>%
    rename(type = modelVarType,
           vars = namesVars,
           bound = X.UPPER.,
           value=modelVarBounds.upper) %>%
    rowwise() %>%
    mutate(boundType = mgsub(type, omprBoundType, mpsUpperBoundType)) %>%
    arrange(vars)

  dfVarBoundLow <- data.frame(modelVarType,
                              namesVars,
                              "LOWER",
                              modelVarBounds$lower,
                              stringsAsFactors = FALSE) %>%
    rename(type = modelVarType,
           vasr = namesVars,
           bound = X.LOWER.,
           value=modelVarBounds.lower) %>%
    rowwise() %>%
    mutate(boundType = mgsub(type, omprBoundType, mpsLowBoundType)) %>%
    filter(value != 0) %>%
    arrange(vars)

  dfVarBounds <- rbind(dfVarBoundUp, dfVarBoundLow) %>%
    arrange(vars) %>%
    mutate(line = str_c(str_pad("", width = 1, side = "left" ,pad = " "),
                        boundType,
                        str_pad("", width = 1, side = "left", pad = " "),
                        "BOUND1",
                        str_pad("", width = 4, side = "left", pad = " "),
                        vars,
                        str_pad(string="", width = (10 - nchar(vars)), side = "left", pad = " " ),
                        formatC(value, digits = 10, format="g")))


  # Write the first line of the file: NAME
  name <- str_c("NAME", str_pad("",11, "right"), modelname)
  write(name, file=file)

  # Write the ROWS section
  write("ROWS", file=file, append = TRUE)

  ## First we write the objective function
  obj <- str_c(str_pad("N", 2, "left"), str_pad("", 2, "right"), "OBJ")
  write(obj, file=file, append = TRUE)

  ## Then we write the rest of the constraints
  cons <- dfCons$row
  write(cons, file=file, append = TRUE)

  # Write the COLUMNS section
  write("COLUMNS", file=file, append = TRUE)

  columns <- dfMatrixA$line
  write(columns, file=file, append=TRUE)

  # Write the RHS section
  write("RHS", file=file, append = TRUE)

  dfConsAux <- dfCons %>%
    filter(rhs != 0)

  rhsLines <- dfConsAux$rhsLine
  write(rhsLines, file=file, append=TRUE)

  # Write the BOUNDS section
  write("BOUNDS", file=file, append = TRUE)

  boundLines <- dfVarBounds$line
  write(boundLines, file=file, append=TRUE)

  # Write ENDATA
  write("ENDATA", file=file, append = TRUE)

}
