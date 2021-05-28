import { useMediaQuery } from "@chakra-ui/react";
import { useState, useEffect } from "react";

const useScreenWiderThanBreakpoint = (width: number) => {
  const [isWide, setIsWide] = useState(true);
  const [moreThan720px] = useMediaQuery(`(min-width: ${width}px)`);

  useEffect(() => {
    setIsWide(moreThan720px);
  }, [moreThan720px]);

  return isWide;
};

export default useScreenWiderThanBreakpoint;
