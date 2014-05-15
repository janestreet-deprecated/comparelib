## 109.60.00

- Fixed a type error in `with compare` of polymorphic variant inclusions.

## 109.27.00

- Changed how `with compare` treats `option`'s so that `None < Some`,
  like `Pervasives.compare`.

    Previously, `with compare` had treated `Some < None`.

## 109.10.00

- Improved error messages in presence of GADTs.

