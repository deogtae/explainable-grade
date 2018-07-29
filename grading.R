# Copyright (c) 2018 by 김덕태 & 고등지능기술원. All rights reserved. 
# 이 저작물은 크리에이티브 커먼즈 저작자표시 2.0 대한민국 라이선스에 따라 이용할 수 있습니다. http://creativecommons.org/licenses/by/2.0/kr/ 

## 배점/문제 등급 파일 읽기

set.seed(20180628)
problem_grade <- read.csv("problem_grade.csv", sep="", fileEncoding="UTF-8", row.names=1, check.names=F, colClasses="character", na.strings="")
problem_grade[,] <- lapply(problem_grade, as.numeric)
str(problem_grade)

if (sum(problem_grade["배점",]) != 100
    || !all(problem_grade["배점",] == colSums(problem_grade[c("D","C","B","A","S"),])))
    stop("배점 파일 오류")

grade_score <- rowSums(problem_grade[c("D","C","B","A","S"),])
grade_score

## 등급 기준 원점수 계산

grade_weights <- read.csv("grade_weights.csv", sep="", fileEncoding="UTF-8", row.names=1, check.names=F, colClasses=c("character", "numeric", "numeric", "numeric", "numeric"), na.strings="")
grade_weights

standard_score_thresholds <- double()
for (grade in c("D", "C", "B", "A", "S")) {
    standard_score_thresholds[grade] <- (grade_score["D"] * grade_weights[grade, "D"] + grade_score["C"] * grade_weights[grade, "C"] + grade_score["B"] * grade_weights[grade, "B"] + grade_score["A"] * grade_weights[grade, "A"]) / 100
}
standard_score_thresholds
if ((standard_score_thresholds["D"] > standard_score_thresholds["C"]) |
    (standard_score_thresholds["C"] > standard_score_thresholds["B"]) |
    (standard_score_thresholds["B"] > standard_score_thresholds["A"]) |
    (standard_score_thresholds["A"] > standard_score_thresholds["S"]))
    stop("등급별 기준 원점수 오류")

## 채점 파일 읽기, 검증

scores <- read.csv("score.csv", sep="", fileEncoding="UTF-8", row.names=1, check.names=F, colClasses="character", na.strings="")
scores
str(scores)
student_num <- nrow(scores)
problem_num <- ncol(problem_grade)
if (problem_num != ncol(scores) - 2)
    stop("문항 개수 오류")
missed <- rowSums(is.na(scores)) > 0
if (!all(missed == (scores[,3]=="대체" | scores[,3]=="결시")))
    stop("대체/결시 검증 오류")

## 표준 점수 계산

standard_scoring <- function(score, thresholds) {
    if (score >= thresholds["S"])
        return(100)
    if (score >= thresholds["A"])
        return(80 + ((score - thresholds["A"]) / (thresholds["S"] - thresholds["A"])) * 20)
    if (score >= thresholds["B"])
        return(60 + ((score - thresholds["B"]) / (thresholds["A"] - thresholds["B"])) * 20)
    if (score >= thresholds["C"])
        return(40 + ((score - thresholds["C"]) / (thresholds["B"] - thresholds["C"])) * 20)
    if (score >= thresholds["D"])
        return(20 + ((score - thresholds["D"]) / (thresholds["C"] - thresholds["D"])) * 20)
    return((score / thresholds["D"]) * 20)
}

grade <- scores
grade[3:ncol(grade)] <- NA_real_
grade["총점"] <- NA_real_
grade["표준점수"] <- NA_real_
grade <- grade[c(1:2, length(grade)-1, length(grade), 3:(2+problem_num))]
grade[scores[,3]=="결시", "총점"] <- 0
grade[scores[,3]=="결시", "표준점수"] <- 0
grade[scores[,3]=="결시", 5:(4+problem_num)] <- 0
grade
normals <- which(!missed)

for (i in normals) {
    total_score <- 0
    for (n in names(scores)[3:(2+problem_num)]) {
        grade[i, n] <- as.numeric(scores[i, n])
        total_score <- total_score + grade[i, n]
    }
    grade[i,"총점"] <- total_score
    grade[i,"표준점수"] <- round(standard_scoring(total_score, standard_score_thresholds), digits=1)
}

## 통계 분석

correctness <- round(colMeans(grade[normals, 5:(4+problem_num)], na.rm=T) / problem_grade["배점",] * 100, digits=1)

n <- nrow(grade) +1
grade[n,] <- rep("", ncol(grade))
n <- n+1
grade[n,] <- c("", "정답율(미응시 제외)(%)", "", "", correctness)
n <- n + 1
total_scores <- as.numeric(grade[normals, "총점"])
grade[n,] <- c("", "평균", round(mean(total_scores), digits=1), rep("", problem_num+1))
n <- n + 1
grade[n,] <- c("", "중간", round(median(total_scores), digits=1), rep("", problem_num+1))
n <- n + 1
grade[n,] <- c("", "최소", round(min(total_scores), digits=1), rep("", problem_num+1))
n <- n + 1
grade[n,] <- c("", "최대", round(max(total_scores), digits=1), rep("", problem_num+1))
n <- n + 1
grade[n,] <- c("", "4분위값", round(quantile(total_scores), digits=1), rep("", problem_num-3))
n <- n + 1
grade[n,] <- c("", "표준편차", round(sd(total_scores) * sqrt((length(total_scores)-1)/length(total_scores)), digits=1), rep("", problem_num+1))
n <- n + 1
grade[n,] <- c("", "등급", "D", "C", "B", "A", "S", rep("", problem_num-3))
n <- n + 1
grade[n,] <- c("", "문제 등급별 총점", grade_score["D"], grade_score["C"], grade_score["B"], grade_score["A"], grade_score["S"], rep("", problem_num-3))
n <- n + 1
grade[n,] <- c("", "등급 기준 원점수", standard_score_thresholds["D"], standard_score_thresholds["C"], standard_score_thresholds["B"], standard_score_thresholds["A"], standard_score_thresholds["S"], rep("", problem_num-3))
summary(total_scores)
grade
tail(grade, n=9)
hist(total_scores, breaks=seq(0,100,10), right=F, main="원점수 히스토그램", xlab="원점수", ylab="빈도")
standard_score <- as.numeric(grade[normals, "표준점수"])
hist(standard_score, breaks=seq(0,100,10), right=F, main="표준점수 히스토그램", xlab="표준 점수", ylab="빈도")

## 채점 파일 저장

write.csv(grade, "grade.csv", row.names=F)
svg("total_score_histogram.svg", family="Gulim")
hist(total_scores, breaks=seq(0,100,10), right=F, main="원점수 히스토그램", xlab="원점수", ylab="빈도")
dev.off()
svg("standard_score_histogram.svg", family="Gulim")
hist(standard_score, breaks=seq(0,100,10), right=F, main="표준점수 히스토그램", xlab="표준 점수", ylab="빈도")
dev.off()

## 공개용 채점 파일 저장

grade_pub <- grade[2:ncol(grade)]
grade_pub[1:student_num, "학번"] <- sapply(grade_pub[1:student_num, "학번"], function(x) return(substr(x, nchar(x)-3, nchar(x))))
indices_pub <- sample(student_num)
grade_pub <- rbind(grade_pub[indices_pub,], grade_pub[-indices_pub,])
write.csv(grade_pub, "grade_public.csv", row.names=F)
