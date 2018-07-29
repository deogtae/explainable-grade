# Copyright (c) 2018 by 김덕태 & 고등지능기술원. All rights reserved. 
# 이 저작물은 크리에이티브 커먼즈 저작자표시 2.0 대한민국 라이선스에 따라 이용할 수 있습니다. http://creativecommons.org/licenses/by/2.0/kr/ 

import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

## 배점/문제 등급 파일 읽기

problem_grade = pd.read_csv("problem_grade.csv", sep="\s+", encoding="UTF-8", header=0, index_col=0, dtype=str, na_values="")
problem_grade = pd.DataFrame(problem_grade, dtype=np.float)
problem_grade.info()
problem_grade

if sum(problem_grade.loc["배점",:]) != 100 or not all(problem_grade.loc["배점",:] == np.sum(problem_grade.loc[["D","C","B","A","S"],:], axis=0)):
    stop("배점 파일 오류")

grade_score = np.sum(problem_grade.loc[["D","C","B","A","S"],:], axis=1)
grade_score

## 등급 기준 원점수 계산

grade_weights = pd.read_csv("grade_weights.csv", sep="\s+", index_col=0, dtype={"grade":str, "D":np.float, "C":np.float, "B":np.float, "A":np.float}, na_values="")
grade_weights

standard_score_thresholds = dict()
for grade in ["D", "C", "B", "A", "S"]:
    standard_score_thresholds[grade] = (grade_score["D"] * grade_weights.loc[grade, "D"] + grade_score["C"] * grade_weights.loc[grade, "C"] + grade_score["B"] * grade_weights.loc[grade, "B"] + grade_score["A"] * grade_weights.loc[grade, "A"]) / 100
standard_score_thresholds
if ((standard_score_thresholds["D"] > standard_score_thresholds["C"]) or
    (standard_score_thresholds["C"] > standard_score_thresholds["B"]) or
    (standard_score_thresholds["B"] > standard_score_thresholds["A"]) or
    (standard_score_thresholds["A"] > standard_score_thresholds["S"])):
    stop("등급별 기준 원점수 오류")

## 채점 파일 읽기, 검증

scores = pd.read_csv("score.csv", sep="\s+", encoding="UTF-8", index_col=0, dtype=str, na_values="")
scores
scores.info()
student_num = scores.shape[0]
problem_num = problem_grade.shape[1]
if problem_num != scores.shape[1] - 2:
    stop("문항 개수 오류")
missed = np.sum(pd.isnull(scores), axis=1) > 0
if not (all(missed == ((scores.iloc[:,2]=="대체") | (scores.iloc[:,2]=="결시")))):
    stop("대체/결시 검증 오류")

## 표준 점수 계산

def standard_scoring(score, thresholds):
    if score >= thresholds["S"]:
        return 100
    if score >= thresholds["A"]:
        return 80 + ((score - thresholds["A"]) / (thresholds["S"] - thresholds["A"])) * 20
    if score >= thresholds["B"]:
        return 60 + ((score - thresholds["B"]) / (thresholds["A"] - thresholds["B"])) * 20
    if score >= thresholds["C"]:
        return 40 + ((score - thresholds["C"]) / (thresholds["B"] - thresholds["C"])) * 20
    if score >= thresholds["D"]:
        return 20 + ((score - thresholds["D"]) / (thresholds["C"] - thresholds["D"])) * 20
    return (score / thresholds["D"]) * 20

grade = scores.copy()
grade.iloc[:,2:grade.shape[1]] = float('nan')
grade["총점"] = ""
grade["표준점수"] = ""
grade = pd.concat([grade.iloc[:,[0, 1, grade.shape[1]-2, grade.shape[1]-1]], grade.iloc[:,2:(2+problem_num)]], axis=1)
grade.loc[scores.iloc[:,2].values =="결시", "총점"] = 0
grade.loc[scores.iloc[:,2]=="결시", "표준점수"] = 0
grade.loc[scores.iloc[:,2]=="결시", 4:(4+problem_num)] = 0
grade
normals = np.where(~missed)[0]

for i in normals:
    total_score = 0
    for n in scores.columns[2:(2+problem_num)]:
        grade.iloc[i][n] = float(scores.iloc[i][n])
        total_score = total_score + grade.iloc[i][n]
    grade.iloc[i]["총점"] = total_score
    grade.iloc[i]["표준점수"] = round(standard_scoring(total_score, standard_score_thresholds), 1)

## 통계 분석

correctness = round(np.mean(grade.iloc[normals][scores.columns[2:(2+problem_num)]], axis=0) / problem_grade.loc["배점",:] * 100, ndigits=1)

n = grade.shape[0]
grade = grade.append([pd.Series(index=grade.columns)] * 11, ignore_index=True)
grade.iloc[n,:] = np.repeat("", grade.shape[1])
n = n+1
grade.iloc[n,:] = np.concatenate([["", "정답율(미응시 제외)(%)", "", ""], ["%g" % x for x in correctness]])
n = n + 1
total_scores = pd.Series(grade.loc[normals, "총점"], dtype=float)
grade.iloc[n,:] = np.concatenate([["", "평균", "%g" % round(np.mean(total_scores), ndigits=1)], np.repeat("", problem_num+1)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "중간", "%g" % round(np.median(total_scores), ndigits=1)], np.repeat("", problem_num+1)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "최소", "%g" % round(np.min(total_scores), ndigits=1)], np.repeat("", problem_num+1)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "최대", "%g" % round(np.max(total_scores), ndigits=1)], np.repeat("", problem_num+1)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "4분위값"], ["%g" % x for x in np.percentile(total_scores, [0, 25, 50, 75, 100]).round(decimals=1)], np.repeat("", problem_num-3)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "표준편차", "%g" % round(np.std(total_scores), ndigits=1)], np.repeat("", problem_num+1)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "등급", "D", "C", "B", "A", "S"], np.repeat("", problem_num-3)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "문제 등급별 총점", "%g" % grade_score["D"], "%g" % grade_score["C"], "%g" % grade_score["B"], "%g" % grade_score["A"], "%g" % grade_score["S"]], np.repeat("", problem_num-3)])
n = n + 1
grade.iloc[n,:] = np.concatenate([["", "등급 기준 원점수", "%g" % standard_score_thresholds["D"], "%g" % standard_score_thresholds["C"], "%g" % standard_score_thresholds["B"], "%g" % standard_score_thresholds["A"], "%g" % standard_score_thresholds["S"]], np.repeat("", problem_num-3)])
total_scores.describe()
grade
grade.tail(9)

import sys
if sys.platform == 'win32':
    matplotlib.rc("font", family="Malgun Gothic")
    #matplotlib.rc("font", family="New Gulim") # SVG 파일로 저장하는 경우, 한글이 안보임.
if sys.platform == 'darwin':
    matplotlib.rc("font", family="AppleGothic")

plt.hist(total_scores, bins=range(0,101,10))
plt.title("원점수 히스토그램")
plt.xlabel("원점수")
plt.ylabel("빈도")
plt.savefig("total_score_histogram.svg")

plt.figure()
standard_score = pd.Series(grade.iloc[normals]["표준점수"], dtype=float)
plt.hist(standard_score, bins=range(0,101,10))
plt.title("표준점수 히스토그램")
plt.xlabel("표준 점수")
plt.ylabel("빈도")
plt.savefig("standard_score_histogram.svg")

## 채점 파일 저장

import csv
for row in grade.index:
    for col in grade.columns:
        if type(grade.loc[row,col]) == float or type(grade.loc[row,col]) == np.float64:
            grade.loc[row,col] = "%g" % grade.loc[row,col]
grade.to_csv("grade.csv", index=False, quoting=csv.QUOTE_ALL, float_format="%g")

## 공개용 채점 파일 저장

grade_pub = grade.iloc[:,1:grade.shape[1]].copy()
grade_pub.iloc[0:student_num,0] = grade_pub.iloc[0:student_num,0].apply(lambda x: x[-4:len(x)])
np.random.seed(20180628)
indices_pub = np.random.choice(student_num, student_num, replace=False)
grade_pub = pd.concat([grade_pub.iloc[indices_pub], grade_pub.iloc[student_num:]])
grade_pub.to_csv("grade_public.csv", index=False, quoting=csv.QUOTE_ALL, float_format="%g")
