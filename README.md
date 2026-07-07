## 컬럼타입별 비교
### Geometry
* 오라클 고유의 타입
1. 컬럼값 읽기
  * `oracle.spatial.geometry.JGeometry.load` 사용

2. 비교
  * `oracle.spatial.geometry.JGeomety` 는 nested-struct 타입
  * Type, RSID 비교후 Odridates를 recursive하게 비교
  * Odridates는 실수값의 배열을 포함

``` scala
// 같은 값을 입력해도, 타입변환으로 인한 오차발생함(ojdbc의 이슈)
// 이를 보정하게 위해 허용오차 적용

// 기본 tolerance ---------------------------------
// 1e-15  double precision 한계
// 1e-12  JVM/ojdbc 흔한 오차
// 1e-9   generous 오차 (1000배 여유)
// 1e-6   GIS meter 수준 (≈ mm~cm)
val DEFAULT_ABS_TOL = 1e-9      // 절대 오차
val DEFAULT_REL_TOL = 1e-12     // 상대 오차
```
### XML
* XML비교는 3가지 방법이 가능
  1. 문자열 비교 (틀릴 가능성 매우 높으나 가장빠름) ~> 채택하면 곤란
  2. 무의미한 요소 제거후 문자열 비교 ( 순서가 달라질 경우 틀린 결과)
  3. XML구조해석후 비교( 정확하나, 비용/시간 과다)

* DBSync는 2의 방법을 채택함. (속도와 정확성 간의 타협)

### ROWID, BFILE
* 동기화 도구의 목적에 따라 문자열 비교를 함.
* 비교하지 않는 게 맞는 건지 애매함. Oracle전문가의 의견 필요




  
  

