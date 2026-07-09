# Internals
## 컬럼타입별 비교
### Geometry
* **오라클 고유의 타입**
1. 컬럼값 읽기
  * `oracle.spatial.geometry.JGeometry.load` 사용

2. 비교
  * `oracle.spatial.geometry.JGeomety` 는 nested-struct 타입
  * Type, RSID 비교후 Odridates를 recursive하게 비교
  * Odridates는 실수값의 배열을 포함

``` scala
// 같은 값을 입력해도, 타입변환으로 인한 오차발생함(ojdbcbug?)
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
* SQL:2003의 표준 컬럼타입(ISO/IEC 9075-14:2023)
* XML비교는 3가지 방법이 가능
  1. 문자열 비교 (틀릴 가능성 매우 높으나 가장빠름) ~> 채택하면 곤란
  2. **무의미한 요소 제거후 문자열 비교** ( 순서가 달라질 경우 틀린 결과)
  3. XML구조해석후 비교( 정확하나, 비용/시간 과다)

* DBSync는 2의 방법을 채택함. (속도와 정확성 간의 타협)

### ROWID, BFILE
* 동기화 도구의 목적에 따라 문자열 비교를 함.
* 비교하지 않는 게 맞는 건지 애매함. Oracle전문가의 의견 필요

### 실수형 오라클 고유타입 Issue
* 아래는 오라클 고유의 컬럼타입으로, 처리 중 side-effect가 발생할 수 있다.

### 1. BINARY_FLOAT
* ojdbc API내의 형변환으로 **10^-7이내의 오차발생**할 수 있음.

### BINARY_DOUBLE
* ojdbc setDouble함수의 제한으로 **overflow 발생할 수 있음.**

```
오버플로우 발생 범위
value > 9.99999999999999E+125 또는
value < -9.99999999999999E+125
```

## schema 정보 읽어오는 sql
DBSync initialize시에 schema읽어오는 동작.

* ojdbc API이용하는 경우 (아래).
  - 처리시간이 지나치게 오래 걸리는 경우가 있어서 아래의 sql방식으로 수정함.
```scala
// meta: DatabaseMetaData
meta.getIndexInfo(null, schema, table, true, false)
meta.getPrimaryKeys(null, schema, table)
```

* primary key
```sql
      SELECT cols.column_name, cols.position
       FROM all_constraints cons
       JOIN all_cons_columns cols
         ON cons.constraint_name = cols.constraint_name
        AND cons.owner = cols.owner
      WHERE cons.constraint_type = 'P'
        AND cons.owner = ?
        AND cons.table_name = ?
```
* unique key
```sql
      SELECT ind.index_name, col.column_name, col.column_position
       FROM all_indexes ind
       JOIN all_ind_columns col
         ON ind.index_name = col.index_name
        AND ind.owner = col.index_owner
      WHERE ind.uniqueness = 'UNIQUE'
        AND ind.table_owner = ?
        AND ind.table_name = ?
```
### 허용오차
* 실수, 시각 등의 비교에 허용오차 설정가능하도록 구현되어 있으나  
* Database간의 동기화에 비추어, **UI상에서 허용오차를 지정기능은 빠져있음**.

## 비교, 적용작업
### 적용실패의 rollback

* 이론상으로는 하나의 테이블에 대한 전체 변경(적용)작업은 하나의 transaction으로 처리되어야 함.
* 그러나, DB에 가해질 부담을 고려하여 **연속된 부분 transaction으로 적용**하도록 처리했음.
* 따라서 어떤 이유로 변경(적용)작업이 중단된다면 `마지막 부분 transaction`만 rollback됨.

### 비교, 적용작업의 갯수
* **중복 Row가 존재하는 경우,** 비교 갯수와 적용 갯수에 차이가 발생함. (당연한 결과)

### 선후 작업의 연결성
* 비교/적용작업을 연달아 하는 command는 reactive하게 연결되어 있음.
* 적용작업에 필요한 만큼을 비교작업결과를 얻어옴. (**pull-based rate control**)
  
> 시스템 자원을 필요한 최소한을 소비하는 방식으로  
> 대량 Data처리에서 일반적으로 사용되는 권장처리방식임.  
> 표면적으로는 비교/적용작업을 비동기적으로 처리하는 것처럼 보일 수 있음.

# Tips
### Table 내부정렬 시간에 대해서 엿보고 싶을 때 
* `ps` command는 지정한 갯수만큼만 비교작업 진행(defalut: 10개)

```desc
비교작업을 위해 DB가 내부정렬 후,
반환하는 시간을 찍도록 되어 있으니
큰 테이블에 비교시에 ps를 먼저 날려보면
내부 정렬에 소요되는 시간을 볼 수 있다.
```

### j-과 p-의 차이

* p- 계열의 명령
```desc
p- 계열의 명령은
대규모 작업 전에 점검을 하는 용도를 상정하여
선택한 Table들 대상으로 순차작업을 하며
작업내역들을 화면에 출력하도록 함.
```

* j-계열의 명령
```desc
j- 계열의 명령은
병렬처리를 통한 빠른 처리를 목표로 상정하여
동작하도록 구현.

- 병렬처리는 DB에 부하를 더 많이 일으키게 됨.
- 기본 병렬처리는 2, 설정으로 4까지 지정할 수 있음  
  (DB 입장에서는 다량의 batch 작업을 연속적으로 수행하는 셈.
  (DB 설정, 상황에 맞게 조절하면 됨.)
```

