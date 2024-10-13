/* eslint-disable no-eq-null, eqeqeq */

const undefinedImpl = undefined;
export { undefinedImpl as undefined };

export function undefinable(a, r, f) {
  return a === undefined ? r : f(a);
}

export function notUndefined(x) {
  return x;
}
